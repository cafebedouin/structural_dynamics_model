% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause — Antisubordination Reading
 *   domain: legal/constitutional/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the antisubordination reading of the Fourteenth
 *   Amendment's Equal Protection Clause: the clause targets caste-like
 *   subordination of historically oppressed groups rather than racial
 *   classification per se, forbidding state action that entrenches hierarchy
 *   and permitting state action that dismantles it. Under this reading the
 *   clause operates as a standing constitutional discipline — courts police
 *   the entrench/dismantle line, subordinated communities hold an enforceable
 *   federal guarantee plus a permitted path to redress, and historically
 *   advantaged groups bear the costs of remediation with their
 *   equal-protection recourse foreclosed by construction. Epsilon referent:
 *   the standing arrangement under contest is the antisubordination doctrinal
 *   arrangement itself as operated across the interval, assessed by the
 *   reading's own lights — NOT the fully realized post-caste endgame, which
 *   would drive epsilon toward zero and is precisely the referent error the
 *   framework forbids for advocacy readings. The residual 0.28 reflects what
 *   the reading itself must concede: real transfers from a defined payer
 *   class, foreclosed claims, enforcement overhead, and discretion risk. This
 *   file is one member of a three-story constraint family (with
 *   equal_protection_kernel__colorblind_reading and
 *   equal_protection_kernel__remedial_reading); the colloquial label 'what
 *   the Equal Protection Clause requires' decomposes into three
 *   epsilon-distinct constraints with different victim sets, beneficiary
 *   sets, and standing rules, linked through network.affects_constraints.
 *   Claim and metrics are independently authored: the claimed type is what I
 *   believe structurally true of this arrangement; the metrics describe its
 *   observed operation, including its decayed present state.
 *
 * KEY AGENTS:
 *   - subordinated_caste_communities: primary beneficiary (organized/trapped) — collects the anticaste shield and the permitted remediation; cannot exit their social position regardless
 *   - historically_advantaged_groups: primary payer (powerful/constrained) — bears remedial burdens with litigation exit closed by the reading's standing rule
 *   - federal_judiciary: agenda setter (institutional/constrained) — administers the entrench/dismantle line; collects doctrinal authority, bears no program costs
 *   - state_governments_and_public_universities: dual-positioned agenda setter/payer (institutional/constrained) — wields the permitted measures while bearing compliance and litigation costs
 *   - civil_rights_advocacy_organizations: secondary beneficiary (organized/identity_locked) — mission-fused maintainer whose standing and funding ride on the arrangement's persistence
 *   - colorblind_constitutional_movement: excluded challenger (powerful/mobile) — its core premise is a category error inside this framework, so it wins only by capturing the interpreter
 *   - constitutional_law_academy: analytical observer (analytical/analytical) — tracks the reading's fortunes and reconstructs founding purpose without a seat in the allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.28).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.55).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause — Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "legal/constitutional/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, 'eff3e38c-1a9e-4154-8d28-db04215e2abd').
narrative_ontology:cs_kernel_codification('eff3e38c-1a9e-4154-8d28-db04215e2abd', fixed_text).
narrative_ontology:cs_authority_grounding('eff3e38c-1a9e-4154-8d28-db04215e2abd', lineage).
narrative_ontology:cs_interpretation_layer_present('eff3e38c-1a9e-4154-8d28-db04215e2abd').
narrative_ontology:cs_reading_relation('eff3e38c-1a9e-4154-8d28-db04215e2abd', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('eff3e38c-1a9e-4154-8d28-db04215e2abd', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_axiom('eff3e38c-1a9e-4154-8d28-db04215e2abd', foundational, equal_protection_targets_subordination_not_classification).
narrative_ontology:cs_axiom_status(equal_protection_targets_subordination_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('eff3e38c-1a9e-4154-8d28-db04215e2abd', equal_protection_targets_subordination_not_classification, deontological).
narrative_ontology:cs_axiom('eff3e38c-1a9e-4154-8d28-db04215e2abd', secondary, dominant_groups_lack_standing_against_remedial_measures).
narrative_ontology:cs_axiom_status(dominant_groups_lack_standing_against_remedial_measures, holdable).
narrative_ontology:cs_axiom_grounding('eff3e38c-1a9e-4154-8d28-db04215e2abd', dominant_groups_lack_standing_against_remedial_measures, conventional).
narrative_ontology:cs_reference_frame('eff3e38c-1a9e-4154-8d28-db04215e2abd', reconstruction_anticaste_guarantee).
narrative_ontology:cs_drift_state('eff3e38c-1a9e-4154-8d28-db04215e2abd', contemporary_post_admissions_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('eff3e38c-1a9e-4154-8d28-db04215e2abd', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, subordinated_caste_communities).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, historically_advantaged_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, state_governments_and_public_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, anticaste_doctrine_of_equal_protection).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, structural_disparity_sociology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically subjected to state-enforced racial caste — descendants of enslaved people, alumni of segregated school systems, descendants of excluded immigrant cohorts. The arrangement guarantees them a federal backstop against state re-entrenchment of hierarchy and channels remedial measures toward them: admissions consideration, contract access, vote-dilution remedies, school-desegregation orders. There is no exit from their social position regardless of what the arrangement does; what the arrangement controls is whether the state's weight sits on the entrenching side or the dismantling side.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, subordinated_caste_communities, beneficiary,
    organized, generational, trapped, national).

% Groups whose members held the advantaged side of the prior caste arrangement and now bear the costs of its dismantling: forgone admissions and contract opportunities, and — decisively under this reading — no equal-protection recourse, because the framework defines their injury as non-cognizable before argument begins. Political mobilization, private-institution substitution, and relocation remain available; the litigation route is closed by the standing rule itself, not by adverse outcomes within it.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_advantaged_groups, payer,
    powerful, biographical, constrained, national).

% The Supreme Court and lower federal bench decide which state actions entrench hierarchy (forbidden) and which dismantle it (permitted), and whose injuries count. They collect doctrinal authority from holding the line and bear none of the program costs their rulings allocate. Individual judges are bound by precedent while serving, but the operative reading itself swaps with personnel turnover — the text has not changed in the interval, the administering majority has, repeatedly.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Legislatures, school districts, and public universities enact and administer the measures the reading permits — desegregation plans, set-asides, holistic admissions — gaining mission-expanding authority under the permission structure while carrying compliance costs, litigation exposure, and federal oversight. When the reading narrows, they lose both the authority and the programs built on it, and absorb the transition costs in both directions.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_governments_and_public_universities, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, state_governments_and_public_universities, payer).

% Litigate to hold and extend the reading's doctrinal positions; their standing, funding base, and institutional purpose depend on the arrangement's persistence. The mission constitutes the organization — staff careers, donor identities, and organizational memory are fused with the anticaste project — so exit is not a live option regardless of individual case outcomes, and losses are absorbed as regrouping rather than departure.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Jurists, scholars, and litigants committed to the categorical colorblind reading of the same text. Inside this arrangement's framework their core premise is a category error, so their objection never registers as constitutional argument — they operate wholly outside its adjudication, publishing, litigating in other forums, and above all competing to capture the interpreter, which is the only lever the framework leaves them.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_constitutional_movement, excluded,
    powerful, generational, mobile, national).

% Scholars tracking the reading's doctrinal fortunes, reconstructing Reconstruction-era purpose from the 1866 debates and enforcement records, and mapping the structure of the reading contest. They hold no seat in the allocation of burdens or benefits beyond scholarly investment in particular positions, and their analyses are available to every other seat.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, constitutional_law_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, subordinated_caste_communities).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Commits state power across all jurisdictions to a single anticaste trajectory: no state may competitively re-entrench racial hierarchy, and subordinated communities hold one enforceable federal guarantee plus a uniformly permitted path to redress, instead of fifty varying regimes ranging from protection to re-subordination.
% TRANSFER_FUNCTION: Moves protection and remedial resources — admissions consideration, contract access, vote-dilution remedies, desegregation orders — toward historically subordinated castes; moves the corresponding costs, in forgone opportunities and foreclosed claims, onto historically advantaged groups, with compliance and litigation costs landing on state institutions.
% ABSENT_VOICES: Colorblind-reading adherents and individual members of advantaged groups who experience remedial measures as personal injury are structurally voiceless within this framework: the standing rule defines their claims as category errors, so their objection never enters the conversation as constitutional argument and exists only as external political pressure aimed at replacing the interpreter.
% DISAPPEARANCE_RATIONALE: If the anticaste discipline vanished overnight, states could re-entrench hierarchy without federal backstop, every remedial program would lose its constitutional cover and collapse under immediate challenge, and subordinated communities would lose both the shield and the permitted redress simultaneously — the admissions, contracting, schooling, and voting landscapes would rearrange within a single election cycle.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to destroy the slave-power caste system: the Black Codes, vagrancy statutes, apprenticeship seizures, and exclusionary militias that re-subordinated freedpeople through facially neutral state law immediately after emancipation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's historicity is corroborated entirely outside any modern beneficiary set: the 1866 congressional debates, the Civil Rights Act of 1866, and Freedmen's Bureau enforcement records document the state-entrenched caste the provision answered. On current status, outside attestation splits: residential and school segregation indices, intergenerational wealth-gap studies, and audit discrimination research attest persistent structural disparity, while the current Court majority's opinions attest the problem is solved and the clause forbids the cure. No single source outside the benefiting parties settles the question — hence contested rather than live or dead.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.28 (interval end) because the arrangement's operative transfer intensity has decayed to roughly a quarter of its mid-century peak as the rival colorblind reading captured the interpreter; the series peaks at 0.42 during the busing and set-aside era, dips through the strict-scrutiny narrowing, bumps at the partial revival, and declines after the admissions repudiation. Suppression is 0.55: the arrangement's distinctive coercive content is the standing foreclosure — a defined class of claims is dismissed as category errors before argument — while political mobilization, jurisdictional arbitrage, and amendment remain open, so suppression is targeted rather than total. Theater_ratio rises monotonically from 0.12 to 0.34: as operative force shrank, ceremonial invocation (dissents, academic discourse, anniversary rhetoric) grew as a share of the arrangement's activity — the leading indicator of a piton-ward drift if the founding problem resolves dead. All three series share one time grid (decadal points 0-60, anchored to 1964 Civil Rights Act era through the post-admissions-repudiation present) so every metric is authored at every examined point. The extractiveness oscillation (rise, narrowing, revival, decline) is a court-composition cycle, not noise: intermittent enforcement driven by interpreter turnover is a structural feature of interpretive constraints, and the base_properties scalars are measured at the interval end state. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and national scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from the same fixed text. From the payer seat (historically_advantaged_groups), the arrangement is an enforced asymmetric burden with the courtroom door closed by definition — the harshest available experience of it. From the beneficiary seat (subordinated_caste_communities), the same structure is a shield plus long-deferred correction finally permitted — the mildest. From the agenda_setter seat (federal_judiciary), it is an administrable doctrinal line that confers authority and costs nothing to operate. The excluded colorblind seat experiences it as category-error enforcement that cannot be answered on the merits, only escaped by capturing the interpreter. The engine computes these per-seat divergences from the power, exit, and role data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the subsidy side: subordinated_caste_communities derive low directionality (they collect protection and remediation; their trapped exit does not push them toward the target pole because they sit on the receiving side of the transfer), and civil_rights_advocacy_organizations derive low directionality as incidental organizational collectors. The victim declaration drives the target side: historically_advantaged_groups derive high directionality — constrained rather than arbitrage-grade exit keeps them near but not at the full-target pole. The institutional seats (federal_judiciary, state_governments_and_public_universities) declare no beneficiary or victim membership, so the canonical fallback would place them near symmetric; their true relationship is administering mild beneficiary — the judiciary collects doctrinal authority at zero program cost, and state actors gain mission-expanding permission that exceeds their compliance burden — hence the directionality override setting institutional seats to 0.35. National spatial scope modestly amplifies effective extraction for the payer seat (harder verification at scale); the engine owns the modifier values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is state-entrenched caste: the Black Codes, vagrancy statutes, and exclusionary institutions that re-subordinated freedpeople through facially neutral law immediately after emancipation. Its status is authored contested, not dead — so no zombie flag fires — but the arrangement's mandatrophy risk is real and measurable: if subordination_persistence_question resolves dead, the arrangement persists as ceremonial anticaste invocation administered by an interpreter increasingly hostile to its operation, which is the classic piton signature (administrator could change it, cost-asymmetry holds, no concentrated maintainer). The classification prevents two mislabels. Against the snare label: the transfer from advantaged groups is avowed, not concealed — the coordination function (an enforceable anticaste commitment solving the majority-entrenchment collective-action problem) is both the arrangement's public justification and its actual operation, so the coordination story is not cover. Against the rope label: the standing foreclosure is genuine asymmetric extraction requiring active judicial enforcement — a defined class pays through the same structure that coordinates everyone else — so pure-coordination labeling would erase the payer seat. The rising theater_ratio series is the early-warning instrument for the first failure mode; the enforcement-sufficiency omega is the instrument for the second.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (antisubordination_reading) of the equal_protection_kernel; what would the sibling readings (colorblind_reading, remedial_reading) change structurally, and where exactly is the disagreement located?',
    'Not resolvable internally: resolved only by framework adoption — interpreter capture (court composition), ratification-era historiography, or constitutional amendment. The disagreement is located in two structural elements: the clause''s target (classification per se versus caste-like subordination) and the standing rule (whether dominant groups may claim the clause against remedial measures).',
    'If the colorblind reading prevails, this constraint''s beneficiary and victim sets invert and its epsilon re-references to a different arrangement entirely; if the remedial reading prevails, dominant-group standing returns and the arrangement becomes gateable through narrow-tailoring review rather than categorically allocated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story instantiates one of three readings of the equal protection kernel; siblings are separate constraint files.').

omega_variable(
    subordination_persistence_question,
    'Is caste-like subordination of historically oppressed groups still live as a structural fact, or has formal equality substantially dissolved it?',
    'Longitudinal disparity data controlling for class position, residential and school segregation indices, intergenerational wealth-gap transmission studies, and audit studies of market discrimination.',
    'If the founding problem is dead, the arrangement loses its object and drifts toward ceremonial persistence (piton trajectory with rising theater_ratio); if live, the coordination function remains intact and the arrangement''s extractive asymmetry stays justified within the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_persistence_question, empirical, 'Whether the founding problem the reading answers still exists.').

omega_variable(
    remedial_burden_harm_status,
    'Do the burdens borne by historically advantaged groups under permitted remedial measures constitute cognizable harm under any defensible frame, or is the reading''s foreclosure of their claims sound?',
    'Comparative analysis of dignity-based versus distributive accounts of constitutional harm; examination of whether any framework can recognize the burden without collapsing the subordination/classification distinction the reading depends on.',
    'If the burden is cognizable, the victim set widens, measured extraction looks more symmetric, and the arrangement reads closer to pure coordination; if not cognizable, the tangled-rope asymmetry stands as the reading intends it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_burden_harm_status, conceptual, 'Boundary of the cognizable-harm set under the reading.').

omega_variable(
    judicial_enforcement_sufficiency,
    'Can judicial enforcement alone hold the antisubordination line against political override (ballot initiatives, appointments-driven reversal, statutory preemption), or does persistence require extra-judicial reinforcement?',
    'Track survival of the reading''s remaining doctrinal positions (vote dilution, disparate-impact regimes, contracting programs) across successive interpreter turnovers; compare jurisdictions with and without statutory entrenchment of remedial programs.',
    'If insufficient, the constraint''s persistence is contingent on interpreter personnel and behaves as a transient interpretive settlement rather than a stable standing arrangement; if sufficient, the enforcement structure is durable across composition cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_enforcement_sufficiency, empirical, 'Whether the enforcement mechanism can sustain the arrangement against capture of the interpreter.').

omega_variable(
    remedial_category_entrenchment_risk,
    'Do race-conscious dismantling measures entrench the very racial categories they mobilize, creating a new hierarchy that the reading''s own anti-entrenchment principle forbids?',
    'Study whether sustained race-conscious programs produce category salience effects that outlast the disparity they target, and whether program sunset designs preserve the dismantling function while releasing the category.',
    'If yes, the reading contains a self-binding limit: its remedial permission is bounded by its own entrenchment prohibition, and the arrangement''s steady state is transitional rather than permanent; if no, the permission is unbounded within the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_category_entrenchment_risk, conceptual, 'Internal tension between the reading''s remedial permission and its anti-entrenchment prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__antisubordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t10, equal_protection_kernel__antisubordination_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_kernel__antisubordination_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__antisubordination_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_kernel__antisubordination_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t50, equal_protection_kernel__antisubordination_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(equa_tr_t50, observed).
narrative_ontology:measurement(equa_tr_t60, equal_protection_kernel__antisubordination_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(equa_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__antisubordination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t10, equal_protection_kernel__antisubordination_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_kernel__antisubordination_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__antisubordination_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_kernel__antisubordination_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t50, equal_protection_kernel__antisubordination_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement_basis(equa_be_t50, observed).
narrative_ontology:measurement(equa_be_t60, equal_protection_kernel__antisubordination_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(equa_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__antisubordination_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t10, equal_protection_kernel__antisubordination_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_kernel__antisubordination_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__antisubordination_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_kernel__antisubordination_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t50, equal_protection_kernel__antisubordination_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(equa_su_t50, observed).
narrative_ontology:measurement(equa_su_t60, equal_protection_kernel__antisubordination_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(equa_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Equal Protection Clause' covers three structurally distinct claims — antisubordination (this file), colorblind, and remedial — with different victim sets (subordination-harmed versus classification-harmed), different beneficiary sets, and incompatible standing rules. Their epsilon values differ because the readings index different normative assessments over the same fixed text, not because the text varies. The fixed text is the common kernel; each reading emits its own constraint. Cross-links are declared in every family member's affects_constraints; the colorbound sibling currently exerts reverse influence on this arrangement's operating environment through interpreter capture, recorded in that file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
