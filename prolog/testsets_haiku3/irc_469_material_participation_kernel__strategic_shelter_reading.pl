% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC §469 Material Participation via Permissive Hour-Counting (Strategic Shelter Reading)
 *   domain: tax/regulatory
 *
 * SUMMARY:
 *   The IRC §469 material participation standard, as administered by the
 *   Treasury and interpreted in case law, permits high-income investors to
 *   qualify passive activity losses for deduction through permissive
 *   hour-counting (the 100-hour threshold and 500-hour safe harbor for real
 *   estate professionals) and grouping elections (Treas. Reg. §1.469-4(d),
 *   allowing taxpayers to aggregate multiple activities for participation
 *   assessment). This reading instantiates a strategic-shelter
 *   interpretation: the permissive thresholds and grouping machinery enable
 *   systematic tax-loss deduction for wealth preservation by investors who
 *   structure light participation claims. The sibling strict-gatekeeper
 *   reading would require verifiable, substantial personal labor (higher
 *   participation bars, narrower grouping) and would dramatically constrain
 *   passive loss deductions. This constraint story models the
 *   strategic-shelter reading only—the permissive operational threshold that
 *   dominates current practice—as a tangled rope: genuine coordination
 *   (clear, predictable standards reduce audit friction), but with asymmetric
 *   extraction (deductions flow to high-income investors; costs shift to
 *   treasury and wage earners). The claim/metric divergence is authored
 *   intentionally: the constraint is claimed as a coordinating rope, but the
 *   metrics describe substantially extractive, actively enforced operation
 *   where the beneficiaries have engineered the permissive boundary precisely
 *   to capture deductions.
 *
 * KEY AGENTS:
 *   - high_income_taxpayers_with_passive_investments: Primary beneficiary; structures participation claims and captures passive loss deductions
 *   - tax_planning_professionals: Primary beneficiary and agenda-setter; designs hour-counting and grouping strategies; sets operational norms
 *   - internal_revenue_service: Dual role (agenda-setter/payer); administers permissive safe harbors; foregoes revenue through deductions
 *   - treasury: Primary payer; bears the cost of foregone revenue from passive loss deductions
 *   - wage_earning_public: Powerless payer; implicitly subsidizes investor deductions through higher effective tax burden
 *   - strict_interpretation_advocates: Excluded; argue for tighter material participation standards but have no binding voice in interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.71).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC §469 Material Participation via Permissive Hour-Counting (Strategic Shelter Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax/regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'e4608157-cfb1-4721-a00e-a844e3d8c7ab').
narrative_ontology:cs_kernel_codification('e4608157-cfb1-4721-a00e-a844e3d8c7ab', formalized).
narrative_ontology:cs_authority_grounding('e4608157-cfb1-4721-a00e-a844e3d8c7ab', extraction).
narrative_ontology:cs_interpretation_layer_present('e4608157-cfb1-4721-a00e-a844e3d8c7ab').
narrative_ontology:cs_reading_relation('e4608157-cfb1-4721-a00e-a844e3d8c7ab', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('e4608157-cfb1-4721-a00e-a844e3d8c7ab', foundational, material_participation_permissive_boundary).
narrative_ontology:cs_axiom_status(material_participation_permissive_boundary, holdable).
narrative_ontology:cs_axiom_grounding('e4608157-cfb1-4721-a00e-a844e3d8c7ab', material_participation_permissive_boundary, empirically_contingent).
narrative_ontology:cs_axiom('e4608157-cfb1-4721-a00e-a844e3d8c7ab', foundational, grouping_election_interpretation_broad).
narrative_ontology:cs_axiom_status(grouping_election_interpretation_broad, holdable).
narrative_ontology:cs_axiom_grounding('e4608157-cfb1-4721-a00e-a844e3d8c7ab', grouping_election_interpretation_broad, conventional).
narrative_ontology:cs_reference_frame('e4608157-cfb1-4721-a00e-a844e3d8c7ab', permissive_objective_materiality_framework).
narrative_ontology:cs_drift_state('e4608157-cfb1-4721-a00e-a844e3d8c7ab', contemporary_practice_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e4608157-cfb1-4721-a00e-a844e3d8c7ab', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_taxpayers_with_passive_investments).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_planning_professionals).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, passive_activity_loss_deduction_beneficiaries).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, treasury).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, internal_revenue_service).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wealthy investors with real estate, partnership, or S-corp passive losses who structure their participation claims to cross material participation thresholds, thereby securing deductions that offset ordinary income and shelter substantial tax liability. Their exit includes legitimate business participation or accepting loss limitations; they choose the former through aggressive compliance at the boundary.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_taxpayers_with_passive_investments, beneficiary,
    powerful, biographical, arbitrage, national).

% CPAs, tax attorneys, and investment advisors who design participation strategies, structure hour-counting arrangements, and elect grouping elections for clients. They benefit through fee-for-service tax planning and grow practices around boundary-straddling compliance. They set the operational norms of what counts as 'participation' by defining protocols clients follow.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_planning_professionals, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, tax_planning_professionals, agenda_setter).

% Investors in syndicated real estate, pass-through entities, and investment partnerships who rely on the permissive reading to qualify their losses as deductible. They depend on the tax deferral these deductions provide; their exit is limited to either accepting loss carryforwards or leaving the investment class entirely.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, passive_activity_loss_deduction_beneficiaries, beneficiary,
    moderate, biographical, constrained, national).

% Administers and interprets the material participation standards; currently grants safe harbors and rebuttals that embody a permissive reading (grouping elections under Treas. Reg. §1.469-4(d), hours-of-service thresholds at 100/500 hours under §469(h)). Collects revenue foregone through deductions claimed under this reading; enforcement is limited by audit capacity and interpretive ambiguity.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, internal_revenue_service, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, internal_revenue_service, payer).

% Bears the cost of foregone revenue from passive loss deductions claimed under the permissive reading. The effective tax base shrinks as higher-income taxpayers utilize grouping elections and hour-counting strategies; the burden shifts to remaining taxpayers or deficit accumulation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, treasury, payer,
    institutional, generational, analytical, national).

% Wage earners and small-business owners who cannot structure passive losses and whose tax bills implicitly subsidize the deductions claimed by higher-income investors. Their exit is purely theoretical: pay the tax they owe or leave the taxed economy. The distributional cost falls invisibly on this seat.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_public, payer,
    powerless, biographical, trapped, national).

% Tax scholars, academic commentators, and reform organizations arguing that the permissive reading exceeds Congressional intent and inflates passive loss claims. They publish criticism and propose legislative tightening but hold no enforcement authority; their exclusion from the interpretive process is what the permissive reading depends on.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, strict_interpretation_advocates, excluded,
    organized, biographical, constrained, national).

% Legislated the passive activity loss limitation in 1986 (IRC §469) with stated intent to prevent high-income taxpayers from sheltering income. The permissive administrative reading and case law have substantially eroded that intent through interpretive drift; Congress has periodically considered but not enacted tighter restrictions.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_taxpayers_with_passive_investments).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, predictable thresholds (100 hours of participation, 500-hour safe harbor for rental real estate professionals, grouping elections) that enable passive investors to structure legitimate business participation claims without ambiguity. Reduces audit friction and planning uncertainty for multi-activity investors.
% TRANSFER_FUNCTION: Moves tax deductions (and deferred tax liability) from the federal treasury and the general wage-earning public to high-income passive investors and their tax advisors, who profit from the deduction-generation and fee-based tax planning the permissive standard enables.
% ABSENT_VOICES: Low-income and wage-earning taxpayers who bear the implicit tax-base-contraction cost have no seat at the interpretation table; strict-reading advocates and academic critics publish their case but have no binding role in regulatory revision; Congress nominally controls the standard but lacks the interpretive authority to override settled Treasury guidance.
% DISAPPEARANCE_RATIONALE: If the permissive reading evaporated and the strict gatekeeper reading replaced it, passive loss deductions would contract sharply (many currently claimed hours would not qualify), high-income investors' tax liability would spike, tax advisory practices would downsize or pivot, and federal revenue would jump—estimated at billions annually. The passive investment market would reorganize around entities that can sustain verifiable, substantial personal labor claims or that accept loss carryforwards.
% FOUNDING_PROBLEM: IRC §469 was enacted in 1986 to prevent high-income taxpayers from using passive losses to shelter earned and portfolio income. The coordination problem was: how to define 'material participation' clearly enough that taxpayers and administrators could determine eligibility without endless litigation, while maintaining the statute's protective intent?
% FOUNDING_PROBLEM_CORROBORATION: The Treasury and IRS interpret the statute permissively and assert the founding problem is solved via clear safe harbors and grouping elections—efficiency and certainty are achieved. Tax scholars and Congressional researchers (e.g., GAO reports, academic critiques in Tax Notes and Virginia Tax Review) attest the founding problem remains live: the permissive reading has eroded the statute's intent to limit shelter, high-income passive-loss claims have risen, and the original protective gate has been substantially breached. The divergence of corroboration marks the contested status.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.48 (1986 enactment, strict initial guidance) to 0.68 (present, entrenched permissive practice) as the interpretation drifts toward maximum-favorable-to-taxpayers boundaries. The 100-hour threshold and grouping elections were designed to reduce compliance friction, but they also lower the bar for claiming participation—a strategically advantageous shift. Suppression mirrors extractiveness (0.55→0.71): the constraint persists by active enforcement of the permissive boundary, blocking strict-interpretation challenges and Congressional tightening attempts. Theater rises from 0.25 to 0.42, indicating that an increasing share of enforcement activity defends the permissive interpretation (defeating challenges, issuing protective rulings) rather than auditing for genuineness of participation. The plateau after year 30 suggests the reading has stabilized in current practice; further drift would require Congressional intervention or a major interpretive reversal (low probability, hence flat end). The one shared time grid ensures all metrics are authored at each examined point: extractiveness and suppression show parallel acceleration; theater rises more slowly, indicating the coordination story (clear standards, predictable treatment) still supplies partial legitimacy alongside the extraction. This profile is consistent with a tangled-rope reading: real coordination benefit (business certainty, audit clarity) captured alongside asymmetric extraction (tax shelter for the wealthy).
 *
 * PERSPECTIVAL GAP:
 *   From the tax-planning professional seat, this is a genuine coordination mechanism: the 100-hour safe harbor eliminates guesswork and enables legitimate business participation claims with confidence. From the Treasury/wage-earner seat, the same mechanism is a permission structure for systematic tax-shelter engineering. The engine computes this divergence from the stakeholder roles and exit options: professionals have arbitrage exit (can shift to other tax strategies if this one closes), while wage earners have trapped exit (must pay the tax system they're burdened by). The perspectival gap is structural, not merely subjective—it flows from differential access to exit and benefit-capture.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income taxpayers with passive investments sit at d≈0.85 (full target of the deduction benefit, but constrained by audit and reporting requirements—not quite d=1.0). Tax professionals sit at d≈0.80 (primary capturer of fee value; beneficiary of the permissive reading's expansion). Treasury and wage earners sit at d≈0.95 and d≈1.0 respectively (pure payers, no countervailing benefit). The IRS occupies d≈0.60 (agenda-setter role, but caught between statutory mandate to enforce 'material participation' and Treasury interpretive guidance that loosens it—duality moderates directionality from pure payer). Strict advocates sit outside the directionality axis (excluded role, no structural integration). The asymmetry—beneficiaries at high d, payers at near-unity d—grounds the extraction claim and satisfies the tangled-rope gate (coordination benefit + asymmetric extraction + active enforcement).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandate conflict by bifurcating interpretation: the statute's protective intent (prevent shelter) persists in the law's text, while the permissive reading (achieve coordination and clarity through generous safe harbors) persists in administrative practice. This is a classic mandatrophy signature: the founding problem (how to prevent shelter while maintaining clarity) has been 'solved' by abandoning the shelter-prevention branch of the mandate and keeping only the clarity branch. The legislative intent is dead (status=dead in founding_problem_status), but the constraint persists because the permissive reading generates coordination benefits that justify its continuation in administrators' eyes and because the beneficiaries have structural power to resist tightening. The theater-ratio climb (0.25→0.42) shows the shift: early enforcement (auditing genuineness) gives way to performative maintenance (defending the permissive interpretation against criticism). Mandatrophy is not declared as a resolved flag because the constraint could be tightened via Congressional action; instead, it is documented in the founding_problem_status=contested and the omega variables addressing interpretive stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_participation_intent_vs_practice,
    'Has the permissive hour-counting and grouping-election reading drifted so far from Congressional intent that it constitutes a de facto repeal of the passive-loss-limitation protective function?',
    'Congressional explicit amendment of §469 or comprehensive Treasury regulatory restatement that either (a) affirms the permissive reading as aligned with intent, or (b) reverses course and tightens participation standards. GAO studies quantifying the revenue cost of permissive interpretations relative to original cost estimates would inform the divergence.',
    'If the reading has substantially repudiated intent, the constraint qualifies as mandatrophy (dead founding problem; constraint persists through beneficiary power, not legislative maintenance). This would argue for reclassification from tangled_rope toward snare (the coordination story is cover; extraction is the primary function). If the reading is consistent with a legitimate evolved interpretation, the tangled_rope classification holds and mandatrophy is contested rather than resolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(material_participation_intent_vs_practice, empirical, 'Whether the permissive reading constitutes interpretive drift or intentional evolution.').

omega_variable(
    strict_gatekeeper_foreclosure,
    'Are the strategic-shelter reading and strict-gatekeeper reading genuinely incompatible (foreclosing one another) within a single legal framework, or do they represent a contested boundary that both readings'' adherents must simply navigate?',
    'Test case or comprehensive regulatory restatement in which the Treasury explicitly commits to ONE reading as the single binding interpretation. Absent that, the readings coexist (via different taxpayers choosing different strategies); presence of that commitment would signal foreclosure.',
    'If foreclosed, the strict reading is eliminated from live legal options and the strategic reading dominates absolutely. If coexisting, both readings persist—the strategic reading captures most high-income taxpayers, while some strict-interpretation adherents continue to file conservatively. The structure of the reading_relations determines the engine''s contention/resolution path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_gatekeeper_foreclosure, conceptual, 'Whether the two readings are logically incompatible or pragmatically differentiated.').

omega_variable(
    grouping_election_strategic_use,
    'Do taxpayers employ grouping elections primarily to reduce administrative burden (genuine coordination benefit), or primarily to inflate participation-hour aggregates and lower the effective participation bar (strategic extraction)?',
    'Audit data showing the distribution of grouping-election usage across taxpayers: if low-income, small-business, and wage-and-salary taxpayers use grouping equally with high-income investors, the coordination hypothesis holds; if high-income investors disproportionately use grouping to aggregate borderline-participation activities, the strategic hypothesis holds.',
    'Coordination primary: the theater_ratio should be lower and the constraint reclassifies toward genuine rope (coordination slightly exceeds extraction). Strategic primary: the theater_ratio accurately reflects performative defense of a permissive boundary, and the tangled_rope classification holds. The engine can detect this through audience analysis (who actually uses the grouping provision) if audit data surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grouping_election_strategic_use, empirical, 'Whether grouping elections serve coordination or strategic tax sheltering.').

omega_variable(
    kernel_reading_coexistence_vs_foreclosure,
    'Is the strategic-shelter reading logically foreclosed by the strict-gatekeeper reading, or do they represent genuinely coexisting contested interpretations?',
    'Structured comparison of the foundational axioms: if the axioms contradict directly (e.g., ''material participation requires substantial personal labor'' vs. ''material participation can be achieved via 100-hour safe harbor''), test whether a single legal framework can hold both without internal contradiction. No framework can hold both simultaneously for the same taxpayer—so the relation is coexists_with (different parties hold different readings in different cases) rather than forecloses (one reading logically eliminates the other from any framework).',
    'Coexists_with (current structure) means both readings persist as live options, and the constraint''s classification is determined by which reading dominates in practice (strategic-shelter dominates, hence tangled_rope). If foreclosed, one reading would be eliminated; the strategic-shelter reading''s dominance would be absolute, potentially strengthening the snare classification (no legitimate alternative interpretation survives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_vs_foreclosure, conceptual, 'Logical structure of the reading pair within the IRC §469 kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc469_shelter_tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(irc469_shelter_tr_t5, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(irc469_shelter_tr_t10, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(irc469_shelter_tr_t15, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(irc469_shelter_tr_t20, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(irc469_shelter_tr_t25, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(irc469_shelter_tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(irc469_shelter_tr_t40, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(irc469_shelter_be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(irc469_shelter_be_t5, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(irc469_shelter_be_t10, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(irc469_shelter_be_t15, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(irc469_shelter_be_t20, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(irc469_shelter_be_t25, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(irc469_shelter_be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(irc469_shelter_be_t40, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(irc469_shelter_su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(irc469_shelter_su_t5, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(irc469_shelter_su_t10, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(irc469_shelter_su_t15, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(irc469_shelter_su_t20, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(irc469_shelter_su_t25, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(irc469_shelter_su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(irc469_shelter_su_t40, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.12).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).

% DUAL FORMULATION NOTE:
% The IRC §469 material participation kernel admits two structurally distinct constraint readings: strategic-shelter (permissive hour-counting and grouping, enabling passive-loss deductions for high-income investors) and strict-gatekeeper (verifiable substantial labor, protective intent preserved). These are sibling constraints, not two measurements of one constraint. The strategic-shelter reading (this story) models the permissive interpretation that dominates current practice; the strict-gatekeeper reading models the tighter standard that Congressional intent apparently endorsed. The ε values differ substantially: strategic-shelter exhibits high extractiveness (0.68), while strict-gatekeeper would show lower extractiveness (approaching a mountain of statutory intent if the tighter standard were in force). The kernel is the contested statutory language; the readings are the conflicting interpretive paths through that kernel. This story links to the sibling via network.affects_constraints and omega variables that document the reading-relation structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strategic_shelter_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
