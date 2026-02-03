# CPS Comprehensive Quality Overhaul Plan

**Strategic Initiative**: Complete quality improvement to prepare for full redesign  
**Commitment Level**: TOTAL  
**Timeline**: Open-ended (no deadline)  
**Approach**: Phased multi-stage overhaul  
**Status**: PLAN MODE - Ready for implementation approval  

---

## Vision

Transform the CPS codebase from "working but complex" to "production-ready for redesign" by:

1. **Normalizing the API** - Complete adoption of ast.nim (formerly normalizedast)
2. **Eliminating duplication** - Remove redundant patterns and logic
3. **Clarifying organization** - Sharp module boundaries, clear responsibilities
4. **Establishing metrics** - Baseline for evaluating redesign improvements
5. **Creating documentation** - Knowledge base for future maintainers and redesign architects

After this quality phase, we'll be positioned to redesign with confidence, targeting:
- Smaller codebase (shrink)
- Better performance (both compile-time and runtime)
- Simpler architecture (fewer moving parts)
- Extensible foundation (ready for new features)

---

## Phase Structure

### **PHASE 0: Foundation (Week 1, ~40 hours)**

**Goal**: Establish the normalized AST as the primary API layer

**Tasks**:

1.1 **Rename normalizedast.nim → ast.nim**
   - Update all imports across codebase (~45-50 files)
   - Update cps.nim main exports
   - Verify all tests pass
   - Rationale: Signals ast.nim IS the primary abstraction

1.2 **Add Missing Distinct Types**
   - Add `Statement = distinct NormNode` (for statements/blocks)
   - Add `Expression = distinct NormNode` (for expressions)
   - Add `TypeSection = distinct NormNode` (for type definitions)
   - Rationale: Enable 15-20% more adoption without breaking changes

1.3 **Document ast.nim Boundary**
   - Create ast.nim module documentation
   - Document what IS normalized (high coverage)
   - Document what ISN'T (genAst, rewrites.nim)
   - Rationale: Set clear expectations and acceptance

1.4 **Establish Metrics Baseline**
   - Lines of code per module
   - ast.nim adoption percentage per module
   - Type safety percentage (no .NimNode conversions)
   - Complexity metrics (cyclomatic, coupling)
   - Test coverage baseline
   - Compilation time baseline
   - Rationale: Measure Phase 1 improvements

1.5 **Create Implementation Guides**
   - "How to migrate a module to ast.nim" guide
   - Common patterns and anti-patterns
   - Before/after examples
   - Rationale: Standardize Phase 1 work

**Deliverables**:
- ✅ ast.nim (renamed, new types added)
- ✅ Updated imports throughout codebase
- ✅ Baseline metrics (spreadsheet/document)
- ✅ Phase 1 implementation guide
- ✅ All tests passing

**Success Criteria**:
- All imports updated
- All tests passing
- Baseline metrics recorded
- New types available for use

---

### **PHASE 1: Quality Overhaul (Weeks 2-4, ~120 hours)**

**Goal**: Achieve comprehensive quality baseline ready for redesign

#### **1.1: API Normalization (Weeks 2, ~40 hours)**

**Focus: environment.nim** (the biggest pain point)

1.1.1 **Redesign environment.nim API**
- Audit all 591 lines
- Identify 8-10 functions returning bare NimNode
- Design NormNode-based equivalents
- Examples of current → target:
  - `objectType(): NimNode` → `objectType(): TypeExpr`
  - `makeType(): NimNode` → `makeType(): TypeSection`
  - `initialization(): NimNode` → `initialization(): Statement`
  - `addAssignment(): NimNode` → `addAssignment(): Statement`

1.1.2 **Update all call sites in transform.nim**
- ~50 call sites using results from environment functions
- Update type annotations
- Remove `.NimNode` conversions
- Add new type handlers where needed

1.1.3 **Standardize API patterns**
- Consistent return types across similar functions
- Clear naming conventions
- Consistent error handling
- Documentation for each function's contract

1.1.4 **Add intermediate types**
- Create helper functions for common conversions
- Add validation at API boundaries
- Document boundary assumptions

**Code Review Gate**: Full environment.nim redesign review
- Verify all functions return NormNode types
- Check all call sites updated
- Validate type safety improvements

#### **1.2: Module Migration (Weeks 2-3, ~40 hours)**

Work through modules in priority order:

**High Priority** (biggest impact):
1. transform.nim - Handle genAst boundary, standardize patterns
2. hooks.nim - ~70% adopted, push to 90%+
3. exprs.nim - Already high adoption, consolidate

**Medium Priority**:
4. callbacks.nim - Low adoption (20%), systematic cleanup
5. defers.nim - Low adoption (20%), systematic cleanup
6. returns.nim - Medium adoption (60%), improve

**Lower Priority** (but important):
7. rewrites.nim - Foundational (can't fully adopt), document boundary
8. spec.nim - Light adoption, clean up usage
9. Other modules - Cleanup and standardize

For each module:
- Remove all `.NimNode` conversions where possible
- Replace with proper ast.nim types
- Consolidate duplicate logic
- Add/update module documentation
- Update/improve tests

#### **1.3: Duplication Elimination (Weeks 3, ~25 hours)**

Identify and consolidate repeated patterns:

1.3.1 **Type creation patterns**
- Consolidate similar type-building functions
- Extract common sequences
- Create helpers for repeated operations

1.3.2 **AST traversal patterns**
- Identify similar tree-walking code
- Extract into reusable functions
- Standardize visitor patterns

1.3.3 **Error handling patterns**
- Consolidate error reporting
- Standardize error context
- Improve error messages

1.3.4 **Code generation patterns**
- Identify repeated generation sequences
- Create generation helpers
- Reduce manual tree construction

#### **1.4: Organization & Cleanup (Weeks 3-4, ~20 hours)**

1.4.1 **Module responsibility clarity**
- Document each module's purpose
- Define clear API boundaries
- Remove cross-cutting concerns
- Create dependency graph

1.4.2 **Code organization within modules**
- Reorder functions by logical grouping
- Group related functionality
- Improve readability
- Add section headers/comments

1.4.3 **Remove dead code**
- Identify unused functions/types
- Remove or relocate
- Clean up commented-out code

#### **1.5: Test Organization & Improvement (Throughout, ~15 hours)**

1.5.1 **Test structure**
- Reorganize test files if needed
- Group related tests
- Add missing test cases

1.5.2 **Test coverage**
- Identify untested code paths
- Add tests for critical functionality
- Maintain/improve coverage %

1.5.3 **Test clarity**
- Rename tests for clarity
- Improve test documentation
- Add examples

**Code Review Gates**: 
- After transform.nim completion (largest file)
- After each major module group
- As-needed during implementation for architectural decisions

#### **1.6: Metrics & Documentation (Weeks 3-4, ~20 hours)**

1.6.1 **Update metrics**
- Measure improvements in each module
- Track adoption increases
- Compare to baseline
- Document findings

1.6.2 **Create knowledge base**
- Before/after documentation for major changes
- Architecture decision records
- Module organization guide
- Refactoring patterns used
- Lessons learned

1.6.3 **Code documentation**
- Update module-level docs
- Document key functions
- Explain non-obvious patterns
- Add design rationale

**Deliverables**:
- ✅ All modules >85% ast.nim adopted
- ✅ environment.nim fully redesigned
- ✅ Zero duplicate patterns (consolidated)
- ✅ Clear module responsibilities
- ✅ Updated metrics (showing improvements)
- ✅ Comprehensive knowledge base
- ✅ All tests passing (improved test organization)

**Success Criteria**:
- ast.nim adoption >85% across all modules
- All .NimNode conversions eliminated (except at genAst boundary)
- Duplication eliminated
- Module dependencies clear
- All tests passing
- Metrics show 25-30% improvement in code quality
- Knowledge base complete for redesign architects

---

## Phase 1 Detailed Module Breakdown

### transform.nim (1,331 lines) - CRITICAL
**Current adoption**: 70%  
**Target adoption**: 85%+  
**Key issue**: genAst boundary forces 17 `.NimNode` conversions  

**Work**:
1. Document the 17 genAst boundary points (unavoidable)
2. Clean up remaining 30% non-adopted code
3. Eliminate duplicate patterns
4. Improve module organization

**Estimated time**: 25-30 hours

### environment.nim (591 lines) - CRITICAL
**Current adoption**: 40%  
**Target adoption**: 85%+  
**Key issue**: 8-10 functions return bare NimNode instead of NormNode types

**Work**:
1. Redesign 8-10 core functions to return NormNode types
2. Add TypeExpr, Statement, TypeSection types
3. Update ~50 call sites in transform.nim
4. Consolidate duplicate code
5. Document API surface

**Estimated time**: 30-35 hours

### hooks.nim (300+ lines) - HIGH PRIORITY
**Current adoption**: 70%  
**Target adoption**: 90%+

**Work**:
1. Push remaining 20% adoption
2. Consolidate duplicate hook patterns
3. Clarify hook registration patterns
4. Improve hook documentation

**Estimated time**: 10-12 hours

### exprs.nim (800+ lines) - HIGH PRIORITY
**Current adoption**: 70%  
**Target adoption**: 85%+

**Work**:
1. Consolidate expression-building patterns
2. Reduce duplication in flattening logic
3. Clarify expression type hierarchy
4. Improve code organization

**Estimated time**: 15-20 hours

### callbacks.nim (200+ lines) - MEDIUM PRIORITY
**Current adoption**: 20%  
**Target adoption**: 85%+

**Work**:
1. Systematic ast.nim adoption
2. Consolidate callback patterns
3. Clarify callback API
4. Improve documentation

**Estimated time**: 10-15 hours

### defers.nim (150+ lines) - MEDIUM PRIORITY
**Current adoption**: 20%  
**Target adoption**: 85%+

**Work**:
1. Systematic ast.nim adoption
2. Consolidate defer patterns
3. Simplify defer handling
4. Improve documentation

**Estimated time**: 8-10 hours

### returns.nim (200+ lines) - MEDIUM PRIORITY
**Current adoption**: 60%  
**Target adoption**: 85%+

**Work**:
1. Complete ast.nim adoption
2. Consolidate return patterns
3. Clarify return value handling
4. Improve documentation

**Estimated time**: 8-10 hours

### spec.nim, rewrites.nim, helpers - LOWER PRIORITY
**Current adoption**: Variable (30-50%)  
**Target adoption**: 85%+ (where possible)

**Work**:
1. Systematic adoption where possible
2. Document boundary constraints
3. Consolidate where feasible
4. Improve organization

**Estimated time**: 15-20 hours combined

---

## Implementation Strategy

### Work Organization

**Approach**: Sequential by priority
1. Phase 0 foundation (all at once)
2. Phase 1 - Start with transform.nim + environment.nim (biggest impact)
3. Then parallel: Move through modules in priority order
4. Metrics/documentation throughout and at end

### Code Review Checkpoints

**As-needed gates** (no fixed schedule):
1. After environment.nim redesign (biggest architectural change)
2. After transform.nim adoption complete (largest file)
3. For architectural decisions/tradeoffs during Phase 1
4. High-risk refactorings in critical modules

**Gate criteria**:
- All tests passing
- Type safety improvements verified
- No new duplication introduced
- Documentation updated
- Metrics show improvement

### Test Maintenance

**Throughout Phase 1**:
- All tests must ALWAYS pass
- Improve test organization as we go
- Add tests for newly exposed edge cases
- Never decrease test coverage

### Commit Strategy

**During Phase 1**:
- Small, logical commits (per function or module section)
- Each commit passes tests
- Clear commit messages explaining changes
- Easy to review and understand progression

---

## Success Metrics & Measurement

### Code Quality Metrics

**Track these before and after Phase 1**:

1. **Type Safety**
   - % modules with 85%+ ast.nim adoption (target: 100%)
   - Number of `.NimNode` conversions (target: <10, only at boundary)
   - Type safety percentage (target: 95%+)

2. **Code Duplication**
   - Number of duplicate patterns (target: 0)
   - Code similarity percentage (measure with tools)
   - Consolidated helper functions created

3. **Complexity**
   - Cyclomatic complexity per module
   - Coupling metrics (external module dependencies)
   - Function length distribution

4. **Organization**
   - Number of modules with clear responsibility
   - Depth of dependency graph
   - API surface clarity

5. **Testing**
   - Test coverage percentage (target: maintain/improve)
   - Test organization quality
   - Edge case coverage

6. **Documentation**
   - Module documentation completeness
   - API documentation quality
   - Architectural clarity

### Performance Metrics

**Baseline and track**:
- Compilation time (with and without cache)
- Memory usage during compilation
- Test suite execution time

### Knowledge Base Completeness

**Documentation checklist**:
- ✅ Architecture overview
- ✅ Module responsibility map
- ✅ API design patterns
- ✅ Refactoring decisions & rationale
- ✅ Design tradeoffs explained
- ✅ Lessons learned
- ✅ "How to add new features" guide
- ✅ Code examples for each pattern

---

## Risk Mitigation

### Risk: Breaking changes during refactoring

**Mitigation**:
- All tests must pass at all times
- Commit frequently with working tests
- Easy to revert if issues arise
- Code review gates catch problems

### Risk: Lost functionality or edge cases

**Mitigation**:
- Comprehensive test organization/improvement
- Add tests for previously untested areas
- Before/after comparison during reviews
- Careful handling of special cases

### Risk: Scope creep during cleanup

**Mitigation**:
- Stay focused on 3 goals: adoption, deduplication, organization
- Defer "nice-to-haves" to future work
- Document out-of-scope ideas for future
- Clear phase boundaries

### Risk: Taking too long

**Mitigation**:
- Open-ended timeline (no time pressure)
- Flexible scope if needed
- Can pause and merge intermediate results
- Can switch to redesign early if saturation point reached

---

## Phase 1 Completion Criteria

Phase 1 is **DONE** when:

1. ✅ All modules >85% ast.nim adopted
2. ✅ Zero duplicate patterns (consolidated)
3. ✅ Clear module responsibilities documented
4. ✅ All tests passing (improved organization)
5. ✅ Metrics show 25-30% improvement
6. ✅ Knowledge base complete for redesign
7. ✅ Code review approved by you
8. ✅ Ready for potential redesign phase

---

## Transition to Phase 2 (Redesign)

**After Phase 1 completion**:

Options:
1. **Immediate transition**: Start redesign with Phase 1 quality as foundation
2. **Pause for review**: Team reviews Phase 1 improvements, then decides
3. **Ship Phase 1**: Merge to main, treat redesign as separate future effort
4. **Assess results**: Evaluate if redesign is still needed based on Phase 1 improvements

**Decision point**: After Phase 1 completion, with metrics and knowledge base in hand

---

## Timeline Estimate

| Phase | Duration | Hours | Key Deliverable |
|-------|----------|-------|-----------------|
| Phase 0 | Week 1 | 40 | ast.nim foundation + metrics baseline |
| Phase 1.1 | Week 2 | 40 | API normalization (environment.nim) |
| Phase 1.2 | Weeks 2-3 | 40 | Module migration (transform, hooks, exprs) |
| Phase 1.3 | Week 3 | 25 | Duplication elimination |
| Phase 1.4 | Weeks 3-4 | 20 | Organization & cleanup |
| Phase 1.5 | Throughout | 15 | Test improvements |
| Phase 1.6 | Weeks 3-4 | 20 | Metrics & knowledge base |
| **Total** | **4 weeks** | **~200 hours** | **Production-ready for redesign** |

**Per-week breakdown**:
- Week 1: 40 hours (foundation)
- Week 2: 60-70 hours (API normalization + module migration starts)
- Week 3: 55-60 hours (module migration continues + duplication elimination)
- Week 4: 35-40 hours (final cleanup + metrics/documentation)

---

## Next Steps

1. **Review this plan** - Do the phases and approach align with your vision?
2. **Approve timeline** - Does 4-week duration work for you?
3. **Confirm code review process** - Should reviews happen as-needed or at phase boundaries?
4. **Clarify knowledge base scope** - Any specific documentation priorities?
5. **Discuss Phase 2** - Do you want to plan the redesign phase now or after Phase 1?

Once approved, I'll:
- Create detailed week-by-week execution schedule
- Generate implementation guides for each module
- Set up metrics tracking spreadsheet
- Begin Phase 0 implementation

---

**Status**: PLAN COMPLETE - Ready for your approval and next steps

