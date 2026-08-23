% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty Partnership Principle — Good Faith Consultation and Active Protection
 *   domain: constitutional/indigenous_rights/post_colonial
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is the founding constitutional document of
 *   Aotearoa New Zealand. The partnership reading — dominant in contemporary
 *   jurisprudence since the 1980s — holds that the Treaty established an
 *   ongoing Crown-Māori partnership requiring good faith consultation and
 *   active protection of Māori interests, despite textual ambiguity between
 *   the English and Māori versions. This reading accepts Crown sovereignty
 *   (parliamentary supremacy) as established but insists it is constrained by
 *   Treaty principles. The Waitangi Tribunal (1975), Treaty settlements
 *   process (1990s-present), and courts have institutionalized this
 *   partnership framework. Māori are coordinated into a consultation
 *   relationship but remain subject to Crown final authority — a genuine
 *   coordination function (ongoing relationship, redress mechanism) fused
 *   with asymmetric extraction (Crown retains ultimate decision power).
 *
 * KEY AGENTS:
 *   - crown_executive: Primary agenda_setter (institutional/arbitrage) — sets consultation policy, controls settlements process, retains final decision authority
 *   - parliament: Co-agenda_setter (institutional/arbitrage) — retains legislative sovereignty, can override partnership principles
 *   - maori_iwi_hapu: Primary payer (organized/constrained) — bear costs of limited self-determination, participate in consultation but lack veto, receive partial redress through settlements
 *   - waitangi_tribunal: Observer/institutional (institutional/analytical) — adjudicates claims, recommends remedies, no binding enforcement power
 *   - settler_population: Excluded (moderate/constrained) — affected by Treaty settlements and resource allocations but not direct parties to partnership
 *   - courts: Observer/institutional (institutional/analytical) — develop partnership jurisprudence, review Crown actions for consistency with principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.58).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.45).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty Partnership Principle — Good Faith Consultation and Active Protection").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous_rights/post_colonial").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, '26034d8b-72e6-4f65-b895-880dc54d8134').
narrative_ontology:cs_kernel_codification('26034d8b-72e6-4f65-b895-880dc54d8134', fixed_text).
narrative_ontology:cs_authority_grounding('26034d8b-72e6-4f65-b895-880dc54d8134', lineage).
narrative_ontology:cs_interpretation_layer_present('26034d8b-72e6-4f65-b895-880dc54d8134').
narrative_ontology:cs_reading_relation('26034d8b-72e6-4f65-b895-880dc54d8134', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('26034d8b-72e6-4f65-b895-880dc54d8134', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('26034d8b-72e6-4f65-b895-880dc54d8134', foundational, treaty_partnership_principle).
narrative_ontology:cs_axiom_status(treaty_partnership_principle, holdable).
narrative_ontology:cs_axiom_grounding('26034d8b-72e6-4f65-b895-880dc54d8134', treaty_partnership_principle, conventional).
narrative_ontology:cs_axiom('26034d8b-72e6-4f65-b895-880dc54d8134', foundational, parliamentary_sovereignty_retained).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_retained, holdable).
narrative_ontology:cs_axiom_grounding('26034d8b-72e6-4f65-b895-880dc54d8134', parliamentary_sovereignty_retained, conventional).
narrative_ontology:cs_axiom('26034d8b-72e6-4f65-b895-880dc54d8134', secondary, active_protection_duty).
narrative_ontology:cs_axiom_status(active_protection_duty, holdable).
narrative_ontology:cs_axiom_grounding('26034d8b-72e6-4f65-b895-880dc54d8134', active_protection_duty, conventional).
narrative_ontology:cs_reference_frame('26034d8b-72e6-4f65-b895-880dc54d8134', partnership_principles_framework).
narrative_ontology:cs_drift_state('26034d8b-72e6-4f65-b895-880dc54d8134', contemporary_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('26034d8b-72e6-4f65-b895-880dc54d8134', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, parliament).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_partnership_principle).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, parliamentary_sovereignty_retained).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, principles_doctrine_constraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the Treaty partnership machinery: sets consultation policy, negotiates settlements, determines quantum and pace of redress. Retains final decision authority over resource allocation, legislation, and policy. Collects the primary benefit — continued sovereignty and legitimacy — while administering the coordination function. Can exit the partnership framework only through constitutional crisis; has arbitrage-grade exit via control of state machinery.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_executive, beneficiary).

% Retains legislative sovereignty — can override Treaty principles by clear statute. Benefits from partnership framework as source of constitutional legitimacy and social stability. Sets the legal boundaries within which consultation operates. Has arbitrage-grade exit (could legislate away partnership principles) but pays political cost.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, parliament, beneficiary).

% Bear the costs of limited self-determination: Crown retains final authority over lands, resources, and taonga. Participate in consultation processes that are mandatory for Crown but advisory in effect. Receive partial redress through settlements (typically 1-3% of lost asset value) and co-governance arrangements. Exit options constrained: constitutional transformation (e.g., Matike Mai) is politically possible but requires Crown agreement; international advocacy has limited domestic enforcement. Identity-locked dynamics: iwi/hapū identity is constituted through relationship to whenua and Treaty; exit from partnership framework risks losing recognized status.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, beneficiary).

% Permanent commission of inquiry established 1975. Adjudicates historical and contemporary Treaty claims, issues reports with recommendations. No binding enforcement power — relies on Crown acceptance and political pressure. Provides the authoritative factual and legal record for settlements. Sees full structure but cannot compel outcomes.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Develop partnership jurisprudence (e.g., Lands case 1987, Broadcasting Assets case 1992). Review Crown actions for consistency with Treaty principles. Can declare inconsistency but cannot strike down legislation (parliamentary sovereignty). Provide judicial enforcement of consultation obligations. Analytical seat with institutional power to shape doctrine.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, courts, observer,
    institutional, generational, analytical, national).

% Affected by Treaty settlements (resource transfers, co-governance) and partnership policies (Māori wards, resource management). Not a direct party to Treaty partnership. Would object to perceived preferential treatment or to insufficient redress depending on political view. Excluded from formal partnership structure but shapes political context for Crown decisions.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settler_population, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, ongoing relationship between Crown and Māori for managing contested sovereignty: mandatory consultation on decisions affecting Māori interests, a claims process for historical grievances (Waitangi Tribunal), and a settlements mechanism transferring assets and establishing co-governance. Solves the coordination problem of how two peoples with different sovereignty claims share a state without perpetual conflict.
% TRANSFER_FUNCTION: Moves final decision authority and resource control from Māori to Crown (sovereignty retention), while moving assets, recognition, and consultation rights from Crown to Māori (settlements, partnership principles). Net transfer favors Crown — settlements represent fraction of lost value; consultation lacks veto.
% ABSENT_VOICES: Māori who reject the partnership framework entirely (e.g., Matike Mai constitutional transformation advocates, independence movements) are structurally excluded — the partnership reading defines the terms of engagement such that only those accepting Crown sovereignty can participate. Also excluded: future generations of both peoples who will inherit the constitutional settlement.
% DISAPPEARANCE_RATIONALE: If the partnership principle and its institutional machinery (Tribunal, settlements, consultation obligations) vanished overnight: Crown would face immediate legitimacy crisis and likely Māori mobilization; Treaty settlements would halt; resource management would revert to unilateral Crown control; courts would lose doctrinal basis for reviewing Crown-Māori relations. The constitutional order would reorganize — likely toward either crown_sovereignty_reading (unconstrained parliamentary supremacy) or rangatiratanga_reading (Māori authority assertion) depending on power dynamics.
% FOUNDING_PROBLEM: How to legitimize Crown sovereignty over Aotearoa while managing Māori rights and interests after 1840, given the Treaty's textual ambiguity and the Crown's failure to honor its protections. The partnership reading was constructed judicially (1980s onward) to resolve the crisis of Crown legitimacy created by Treaty breaches.
% FOUNDING_PROBLEM_CORROBORATION: Crown and Parliament attest the founding problem (legitimizing sovereignty) is live and partnership solves it. Māori claimants (Waitangi Tribunal claimants, iwi leaders) attest the founding problem is misstated — the real problem is Crown's usurpation of rangatiratanga, which partnership manages but does not solve. Independent constitutional scholars (e.g., Moana Jackson, Ani Mikaere) and UN bodies corroborate the Māori view that partnership subordinates tino rangatiratanga. No neutral arbiter exists.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the hybrid structure: Crown extracts sovereignty retention and final authority (extraction) while providing consultation infrastructure and settlement redress (coordination). The 40-year measurement series (1975-2015) shows extraction declining from 0.75 to 0.58 as settlements transfer assets and consultation becomes routinized, but plateauing as parliamentary sovereignty remains intact. Theater ratio (0.42) captures the gap between partnership rhetoric and Crown's retention of ultimate authority — declining from 0.65 as institutions mature but stabilizing above zero because the partnership frame legitimizes Crown sovereignty. Suppression requirement (0.45) shows enforcement machinery (Tribunal, courts, settlements) has reduced but not eliminated the need for active suppression of Māori self-determination claims beyond partnership. Accessibility collapse (0.55) and resistance (0.72) reflect that alternatives (full rangatiratanga, constitutional transformation) remain thinkable and actively pursued.
 *
 * PERSPECTIVAL GAP:
 *   From Crown/Parliament seats: the constraint is genuine coordination — a world-leading indigenous rights framework delivering redress and partnership. From Māori seats: the same structure operates as managed extraction — consultation without veto, settlements at Crown-determined pace and quantum, sovereignty retained. The engine computes this divergence from the structural data; the authored claim (tangled_rope) names the hybrid without adjudicating which seat is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Crown executive and Parliament are structural beneficiaries (d near 0.0-0.2): they collect sovereignty retention and final authority, control the partnership framework, and have arbitrage-grade exit (they designed the system). Māori iwi/hapū are payers (d near 0.7-0.8): they bear costs of constrained authority, participate in a framework they did not design, and have constrained exit (constitutional transformation is politically difficult but not impossible). The Waitangi Tribunal and courts are analytical observers (d=0.5). Settler population is excluded — affected but not seated. Directionality derivation from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership reading prevents mislabeling the Treaty relationship as pure coordination (rope) or pure extraction (snare). The coordination function (ongoing relationship, consultation, settlements) is real and valued by Māori — it is not mere cover. The extraction (Crown sovereignty retention, final authority) is also real and structural — not incidental. Tangled_rope correctly captures that the arrangement must be actively enforced (Tribunal, courts, settlements legislation) and that both coordination and extraction persist. Mandatrophy is not resolved: the founding problem (just Crown acquisition) is dead but the arrangement persists because it now serves ongoing constitutional coordination — a live founding problem has been substituted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint the partnership_reading of the waitangi_sovereignty_allocation kernel, and how do sibling readings structurally differ?',
    'Compare beneficiary/victim structures and extraction profiles across the three readings: crown_sovereignty_reading (Crown beneficiary, Māori victim, high extraction), partnership_reading (Crown + Parliament beneficiaries, Māori payer with partial coordination, moderate extraction), rangatiratanga_reading (Māori beneficiary, Crown payer, low extraction from Māori perspective).',
    'If readings share the same ε referent but produce divergent ε values, they are distinct constraints per ε-invariance; the partnership reading''s ε=0.58 reflects its hybrid coordination/extraction structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel reading identity and structural differentiation from sibling readings').

omega_variable(
    partnership_vs_rangatiratanga_foreclosure,
    'Does the partnership reading''s acceptance of parliamentary sovereignty as ultimate logically foreclose the rangatiratanga reading''s claim of full Māori authority within a single constitutional framework?',
    'Analyze whether a framework adopting partnership principles (constraints on Crown but Crown sovereignty retained) can simultaneously hold rangatiratanga''s premise (Māori retained tino rangatiratanga as full authority). The partnership reading''s structural delta explicitly states ''principles doctrine constrains but does not override parliamentary sovereignty.''',
    'If forecloses, the two readings cannot coexist in one framework — they are mutually exclusive constitutional theories. If coexists_with, they are competing live positions held by different parties within the same system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_vs_rangatiratanga_foreclosure, conceptual, 'Logical relationship between partnership and rangatiratanga readings within a single framework').

omega_variable(
    extraction_measurement_ambiguity,
    'How to measure extraction when the Crown both coordinates (Treaty settlements, consultation infrastructure) and extracts (retained sovereignty, ultimate decision authority)?',
    'Decompose the constraint into coordination sub-constraint (consultation requirements, settlement process) and extraction sub-constraint (parliamentary sovereignty override, final decision power). Measure ε for each component separately per ε-invariance principle.',
    'If inseparable, the authored ε=0.58 represents a blended measure. If separable, two constraint stories should be written: one rope (consultation coordination) and one snare (sovereignty retention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_ambiguity, empirical, 'Whether coordination and extraction components are structurally separable for measurement').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to Māori self-determination, Crown monopoly on coercion) or internalized (Māori acceptance of partnership framework as best available)?',
    'Post-settlement trajectory analysis: if suppression persists after settlement redress and consultation mechanisms are operational, reclassify as partially internalized. Track Māori political mobilization outside partnership channels.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s legitimacy narrative becomes part of the suppression machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in Treaty partnership').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_partnership_tr_t0, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t0, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t10, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t10, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t20, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t20, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t30, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t30, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t40, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(waitangi_partnership_be_t0, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement_basis(waitangi_partnership_be_t0, observed).
narrative_ontology:measurement(waitangi_partnership_be_t10, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(waitangi_partnership_be_t10, observed).
narrative_ontology:measurement(waitangi_partnership_be_t20, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(waitangi_partnership_be_t20, observed).
narrative_ontology:measurement(waitangi_partnership_be_t30, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(waitangi_partnership_be_t30, observed).
narrative_ontology:measurement(waitangi_partnership_be_t40, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(waitangi_partnership_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_partnership_su_t0, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(waitangi_partnership_su_t0, observed).
narrative_ontology:measurement(waitangi_partnership_su_t10, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(waitangi_partnership_su_t10, observed).
narrative_ontology:measurement(waitangi_partnership_su_t20, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(waitangi_partnership_su_t20, observed).
narrative_ontology:measurement(waitangi_partnership_su_t30, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(waitangi_partnership_su_t30, observed).
narrative_ontology:measurement(waitangi_partnership_su_t40, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(waitangi_partnership_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(waitangi_partnership_grid_01, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(class), 0, 0.8).
narrative_ontology:measurement_basis(waitangi_partnership_grid_01, observed).
narrative_ontology:measurement(waitangi_partnership_grid_02, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(class), 40, 0.5).
narrative_ontology:measurement_basis(waitangi_partnership_grid_02, observed).
narrative_ontology:measurement(waitangi_partnership_grid_03, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement_basis(waitangi_partnership_grid_03, observed).
narrative_ontology:measurement(waitangi_partnership_grid_04, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(individual), 40, 0.4).
narrative_ontology:measurement_basis(waitangi_partnership_grid_04, observed).
narrative_ontology:measurement(waitangi_partnership_grid_05, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(organizational), 0, 0.7).
narrative_ontology:measurement_basis(waitangi_partnership_grid_05, observed).
narrative_ontology:measurement(waitangi_partnership_grid_06, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(organizational), 40, 0.45).
narrative_ontology:measurement_basis(waitangi_partnership_grid_06, observed).
narrative_ontology:measurement(waitangi_partnership_grid_07, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(structural), 0, 0.85).
narrative_ontology:measurement_basis(waitangi_partnership_grid_07, observed).
narrative_ontology:measurement(waitangi_partnership_grid_08, waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse(structural), 40, 0.55).
narrative_ontology:measurement_basis(waitangi_partnership_grid_08, observed).
narrative_ontology:measurement(waitangi_partnership_grid_09, waitangi_sovereignty_allocation__partnership_reading, resistance(class), 0, 0.8).
narrative_ontology:measurement_basis(waitangi_partnership_grid_09, observed).
narrative_ontology:measurement(waitangi_partnership_grid_10, waitangi_sovereignty_allocation__partnership_reading, resistance(class), 40, 0.72).
narrative_ontology:measurement_basis(waitangi_partnership_grid_10, observed).
narrative_ontology:measurement(waitangi_partnership_grid_11, waitangi_sovereignty_allocation__partnership_reading, resistance(individual), 0, 0.65).
narrative_ontology:measurement_basis(waitangi_partnership_grid_11, observed).
narrative_ontology:measurement(waitangi_partnership_grid_12, waitangi_sovereignty_allocation__partnership_reading, resistance(individual), 40, 0.7).
narrative_ontology:measurement_basis(waitangi_partnership_grid_12, observed).
narrative_ontology:measurement(waitangi_partnership_grid_13, waitangi_sovereignty_allocation__partnership_reading, resistance(organizational), 0, 0.7).
narrative_ontology:measurement_basis(waitangi_partnership_grid_13, observed).
narrative_ontology:measurement(waitangi_partnership_grid_14, waitangi_sovereignty_allocation__partnership_reading, resistance(organizational), 40, 0.75).
narrative_ontology:measurement_basis(waitangi_partnership_grid_14, observed).
narrative_ontology:measurement(waitangi_partnership_grid_15, waitangi_sovereignty_allocation__partnership_reading, resistance(structural), 0, 0.85).
narrative_ontology:measurement_basis(waitangi_partnership_grid_15, observed).
narrative_ontology:measurement(waitangi_partnership_grid_16, waitangi_sovereignty_allocation__partnership_reading, resistance(structural), 40, 0.7).
narrative_ontology:measurement_basis(waitangi_partnership_grid_16, observed).
narrative_ontology:measurement(waitangi_partnership_grid_17, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(class), 0, 0.7).
narrative_ontology:measurement_basis(waitangi_partnership_grid_17, observed).
narrative_ontology:measurement(waitangi_partnership_grid_18, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(class), 40, 0.5).
narrative_ontology:measurement_basis(waitangi_partnership_grid_18, observed).
narrative_ontology:measurement(waitangi_partnership_grid_19, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(individual), 0, 0.55).
narrative_ontology:measurement_basis(waitangi_partnership_grid_19, observed).
narrative_ontology:measurement(waitangi_partnership_grid_20, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(individual), 40, 0.4).
narrative_ontology:measurement_basis(waitangi_partnership_grid_20, observed).
narrative_ontology:measurement(waitangi_partnership_grid_21, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(organizational), 0, 0.6).
narrative_ontology:measurement_basis(waitangi_partnership_grid_21, observed).
narrative_ontology:measurement(waitangi_partnership_grid_22, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(organizational), 40, 0.45).
narrative_ontology:measurement_basis(waitangi_partnership_grid_22, observed).
narrative_ontology:measurement(waitangi_partnership_grid_23, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(structural), 0, 0.75).
narrative_ontology:measurement_basis(waitangi_partnership_grid_23, observed).
narrative_ontology:measurement(waitangi_partnership_grid_24, waitangi_sovereignty_allocation__partnership_reading, stakes_inflation(structural), 40, 0.5).
narrative_ontology:measurement_basis(waitangi_partnership_grid_24, observed).
narrative_ontology:measurement(waitangi_partnership_grid_25, waitangi_sovereignty_allocation__partnership_reading, suppression(class), 0, 0.75).
narrative_ontology:measurement_basis(waitangi_partnership_grid_25, observed).
narrative_ontology:measurement(waitangi_partnership_grid_26, waitangi_sovereignty_allocation__partnership_reading, suppression(class), 40, 0.45).
narrative_ontology:measurement_basis(waitangi_partnership_grid_26, observed).
narrative_ontology:measurement(waitangi_partnership_grid_27, waitangi_sovereignty_allocation__partnership_reading, suppression(individual), 0, 0.6).
narrative_ontology:measurement_basis(waitangi_partnership_grid_27, observed).
narrative_ontology:measurement(waitangi_partnership_grid_28, waitangi_sovereignty_allocation__partnership_reading, suppression(individual), 40, 0.35).
narrative_ontology:measurement_basis(waitangi_partnership_grid_28, observed).
narrative_ontology:measurement(waitangi_partnership_grid_29, waitangi_sovereignty_allocation__partnership_reading, suppression(organizational), 0, 0.65).
narrative_ontology:measurement_basis(waitangi_partnership_grid_29, observed).
narrative_ontology:measurement(waitangi_partnership_grid_30, waitangi_sovereignty_allocation__partnership_reading, suppression(organizational), 40, 0.4).
narrative_ontology:measurement_basis(waitangi_partnership_grid_30, observed).
narrative_ontology:measurement(waitangi_partnership_grid_31, waitangi_sovereignty_allocation__partnership_reading, suppression(structural), 0, 0.8).
narrative_ontology:measurement_basis(waitangi_partnership_grid_31, observed).
narrative_ontology:measurement(waitangi_partnership_grid_32, waitangi_sovereignty_allocation__partnership_reading, suppression(structural), 40, 0.5).
narrative_ontology:measurement_basis(waitangi_partnership_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__partnership_reading, 0.08).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% Part of waitangi_sovereignty_allocation constraint family. The three readings share the Treaty text as kernel but instantiate different constraints with distinct ε values, beneficiary/victim structures, and coordination/extraction profiles. Partnership reading (this file) ε=0.58, tangled_rope. Crown sovereignty reading ε≈0.75, snare (high extraction, minimal coordination). Rangatiratanga reading ε≈0.25, rope/scaffold (coordination of Māori authority, low extraction from Māori perspective).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
