% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: 1890 Manifesto as Strategic Scope-Ambiguous Institutional Adaptation
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   In 1890, facing federal disincorporation, mass property confiscation
 *   under the Edmunds-Tucker Act, and a blocked path to statehood, Church
 *   President Wilford Woodruff issued the Manifesto advising members to
 *   'refrain from contracting any marriage forbidden by the law of the land.'
 *   The document's carefully hedged language — advising rather than
 *   commanding, addressing new marriages without explicitly voiding existing
 *   ones, framed as inspired counsel rather than unambiguous revelation
 *   revoking prior revelation — let the institution present federal
 *   compliance to outside observers while leaving internal theological status
 *   genuinely unsettled for years. This story treats that scope ambiguity
 *   itself as the constraint under contest: an instrument that let leadership
 *   manage an external crisis without forcing a doctrinal reckoning, at the
 *   cost of members left to navigate an unresolved and personally
 *   consequential question with no stable institutional answer.
 *
 * KEY AGENTS:
 *   - church_hierarchy: sets and later revises the Manifesto's operative meaning — institutional/arbitrage
 *   - rank_and_file_members: bear the ambiguity's practical consequences with no authoritative resolution — powerless/trapped
 *   - plural_families_already_formed: live the inconsistent enforcement of an unresolved instruction — powerless/trapped
 *   - excommunicated_dissenters: punished for taking the prior revelation at face value once compliance was needed — powerless/trapped/excluded
 *   - federal_government: applies the exogenous pressure and accepts ambiguous compliance as sufficient — institutional/analytical
 *   - later_church_leadership: inherits and redeploys the interpretive latitude the ambiguity created — institutional/arbitrage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "1890 Manifesto as Strategic Scope-Ambiguous Institutional Adaptation").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'dce57a49-9845-492b-b8c1-6ea8956081de').
narrative_ontology:cs_kernel_codification('dce57a49-9845-492b-b8c1-6ea8956081de', formalized).
narrative_ontology:cs_authority_grounding('dce57a49-9845-492b-b8c1-6ea8956081de', lineage).
narrative_ontology:cs_interpretation_layer_present('dce57a49-9845-492b-b8c1-6ea8956081de').
narrative_ontology:cs_reading_relation('dce57a49-9845-492b-b8c1-6ea8956081de', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('dce57a49-9845-492b-b8c1-6ea8956081de', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('dce57a49-9845-492b-b8c1-6ea8956081de', foundational, prophetic_authority_admits_strategic_deployment).
narrative_ontology:cs_axiom_status(prophetic_authority_admits_strategic_deployment, holdable).
narrative_ontology:cs_axiom_grounding('dce57a49-9845-492b-b8c1-6ea8956081de', prophetic_authority_admits_strategic_deployment, conventional).
narrative_ontology:cs_axiom('dce57a49-9845-492b-b8c1-6ea8956081de', foundational, scope_ambiguity_is_load_bearing_not_incidental).
narrative_ontology:cs_axiom_status(scope_ambiguity_is_load_bearing_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('dce57a49-9845-492b-b8c1-6ea8956081de', scope_ambiguity_is_load_bearing_not_incidental, instrumental).
narrative_ontology:cs_reference_frame('dce57a49-9845-492b-b8c1-6ea8956081de', continuing_revelation_with_institutional_prudence).
narrative_ontology:cs_drift_state('dce57a49-9845-492b-b8c1-6ea8956081de', post_second_manifesto_1904, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dce57a49-9845-492b-b8c1-6ea8956081de', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_survival_apparatus).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, plural_families_already_formed).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, excommunicated_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, later_church_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and issues the Manifesto's carefully bounded language ('advise... to refrain'), retains the prophetic office's authority to declare what it means, and manages federal relations, statehood negotiations, and property confiscation risk simultaneously. Can revise the operative interpretation later (as it does, repeatedly, over subsequent decades) without admitting the earlier interpretation was wrong.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchy, beneficiary).

% Receive a document whose scope is deliberately unclear as to whether it ends the practice, ends new marriages only, or ends nothing theologically while suspending it practically. Must decide, individually and with no authoritative guidance, whether to abandon existing plural households, contract new ones covertly, or comply fully — each choice carrying different legal, social, and salvific stakes the members did not create and cannot resolve themselves.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    powerless, biographical, trapped, regional).

% Existing wives and children face the practical fallout of ambiguous instruction: some households are quietly maintained, some publicly dissolved, some prosecuted anyway. The ambiguity that protects the institution from a clean admission of doctrinal reversal is lived out by these families as inconsistent, unpredictable exposure to legal and social consequence.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, plural_families_already_formed, payer,
    powerless, biographical, trapped, regional).

% Members who continue plural marriage after the Manifesto, believing it remained a theological duty, are excommunicated once the institution needs to demonstrate compliance to federal authorities. They bear the full cost of an ambiguity that shielded the leadership but not them; their voice — that they took the earlier revelation at its word — plays no role in the sanctioned account.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, excommunicated_dissenters, excluded,
    powerless, biographical, trapped, regional).

% Applies escalating pressure (Edmunds-Tucker Act, property seizure, disincorporation threat, statehood conditionality) that constitutes the exogenous crisis the Manifesto responds to. Accepts the Manifesto as sufficient practical compliance without requiring doctrinal repudiation, effectively ratifying the scope ambiguity as adequate.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, excluded).

% Decades later, inherits an interpretive resource the original ambiguity created: the ability to declare the Manifesto a full doctrinal end (1904 Second Manifesto) or an inspired administrative pause, as institutional needs of the moment require, without ever having to characterize 1890 as either capitulation or revelation in a way that could be falsified against the record.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, later_church_leadership, beneficiary,
    institutional, civilizational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a single instrument that simultaneously satisfies federal demands for practical cessation and preserves internal theological continuity, avoiding a fracture between compliance and doctrine that a clean statement in either direction would have forced.
% TRANSFER_FUNCTION: Moves the cost of interpretive uncertainty from the institution (which retains authority to define the Manifesto's meaning over time) onto individual members and families, who must act under legal and social risk without a stable answer to what they are being asked to believe or do.
% ABSENT_VOICES: Members who continued plural marriage in good-faith reliance on the prior revelation are excommunicated when their reading of the ambiguity conflicts with the institution's current compliance needs; their testimony that the earlier command was unambiguous to them is structurally excluded from the sanctioned narrative, which requires the ambiguity to have always been available.
% DISAPPEARANCE_RATIONALE: If the scope-ambiguous framing had not been available and the institution had been forced to choose cleanly between 'God commanded reversal' and 'we capitulated under duress,' the historical record suggests either a doctrinal schism (a faction rejecting a claimed revelation) or a legitimacy collapse (an admission of coercion undermining the prophetic office). Whether the world would meaningfully 'rearrange' depends on which sibling reading is correct about what the ambiguity was covering — hence contested rather than settled.
% FOUNDING_PROBLEM: The Church faced existential federal pressure — disincorporation, mass property seizure, denial of statehood — that plural marriage practice was triggering, while its own recently reaffirmed revelation (1886 Woodruff vision, decades of preaching) made a flat doctrinal reversal costly to institutional credibility.
% FOUNDING_PROBLEM_CORROBORATION: Institutional historians within later Church scholarship largely corroborate the exogenous-pressure account (federal legislation, financial collapse of Church assets) as the proximate trigger. Independent legal historians of the Edmunds-Tucker era corroborate the severity of the external pressure from outside the institution. However, no source outside the benefiting hierarchy corroborates that the ambiguity itself was a deliberate strategic choice rather than a genuine theological negotiation in real time — that specific claim rests on retrospective institutional and academic interpretation, not on a contemporaneous outside attestation.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, contested).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.52) rather than high: this is not pure rent-extraction, there is a genuine coordination problem (institutional survival against real federal force) that the ambiguity solves. But it is not zero either, because the same ambiguity that solves the coordination problem for leadership imposes a distinct, uncompensated cost — interpretive uncertainty and inconsistent enforcement exposure — onto members who did not choose the ambiguity and cannot resolve it themselves. Suppression starts high (0.65 in 1890, during active federal prosecution and internal enforcement of compliance) and eases somewhat by 1930 as the crisis recedes and later leadership's interpretive latitude becomes more settled practice than contested improvisation. Theater ratio rises over the interval (0.30 to 0.44) as the original crisis-management function fades but the ambiguity is retained and increasingly redeployed for institutional narrative management (1904 Second Manifesto, later historical framing) rather than active crisis response — a drift toward performative maintenance of an interpretive resource whose founding emergency has passed.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy sits near the beneficiary end: it retains authority to declare what the ambiguous document means, at whatever time serves institutional needs, and captures the coordination benefit (survival, eventual statehood, property retention) without bearing the ambiguity's costs itself. Rank-and-file members and existing plural families sit near the target end: trapped exit options (leaving the faith community carries severe social and, for some, family costs; staying means living under unresolved doctrinal status), no voice in defining scope, and inconsistent downstream consequences (some households quietly tolerated, others prosecuted, others excommunicated) that track institutional convenience rather than any principle members could anticipate. Excommunicated dissenters are the sharpest case: their directionality is pushed to full-target because they suffer the concrete, discrete penalty (excommunication) that operationalizes the ambiguity's cost, precisely because they resolved the ambiguity in the direction the institution needed to disavow once compliance had to be demonstrated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — surviving an existential federal assault — is genuinely dead by any reasonable measure after Utah statehood (1896) and the broader collapse of federal anti-polygamy enforcement infrastructure. Yet the interpretive apparatus the Manifesto created (the latitude to declare its own scope after the fact) persisted and was actively redeployed in 1904 and in later historical accounts, well past the crisis that justified it. This is the mandatrophy signature: a mandate (manage the crisis) that outlived its function while the instrument built to execute it (scope ambiguity as a governance tool) remained institutionally useful for unrelated purposes (narrative control, doctrinal flexibility). Classifying this as tangled_rope rather than mountain or pure snare captures that the coordination function was real and time-bound while the extraction (member-borne interpretive risk, inconsistent enforcement) has continued to accrue after the coordination need expired — exactly the divergence the framework is built to detect rather than to reconcile away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_vs_emergent_ambiguity,
    'Was the Manifesto''s scope ambiguity a deliberate strategic choice by 1890 leadership, or did the ambiguity emerge from genuine theological uncertainty that was only retrospectively exploited by later leadership?',
    'Close textual and correspondence analysis of Woodruff''s private journals and First Presidency deliberation records from 1889-1890 for evidence of conscious scope-hedging versus authentic uncertainty about the document''s theological status at the time of drafting.',
    'If deliberate strategic hedging, the hybrid_pragmatic_reading is strongly supported and the tangled_rope classification (genuine coordination plus knowing asymmetric extraction) holds firmly. If the ambiguity was authentic uncertainty only later capitalized on, the constraint drifts toward a scaffold (transitional coordination whose later exploitation is a distinct, subsequent constraint) rather than a tangled_rope from inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_intent_vs_emergent_ambiguity, empirical, 'Whether 1890 leadership consciously engineered the ambiguity or inherited it from genuine doctrinal uncertainty.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three sibling readings of the marriage_commitment_legitimacy kernel disagree — is it about the causal weight of federal pressure, about the authenticity of the 1890 revelatory claim, or about whether the document''s ambiguity was itself an object of institutional design?',
    'This is a committer-frame question, not resolvable by additional historical evidence alone. Each reading privileges a different weighting of the same documentary record. The endogenous_reinterpretation_reading privileges Woodruff''s own testimony of receiving guidance; the exogenous_override_reading privileges the correspondence showing property seizure timelines; the hybrid_pragmatic_reading privileges the specific hedged language choices in the Manifesto text itself as evidence of intentional latitude-preservation.',
    'The three readings are not competing empirical hypotheses resolvable by a single fact — they instantiate genuinely different constraints with different beneficiary/victim structures and different epsilon values. This omega documents that the disagreement is located in which evidentiary strand each reading treats as dispositive, not in a shared fact pattern awaiting resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locates the structural disagreement among the three kernel readings in evidentiary emphasis rather than in a resolvable empirical fact.').

omega_variable(
    member_belief_heterogeneity,
    'How many rank-and-file members in 1890-1904 actually experienced the Manifesto''s ambiguity as a live personal dilemma, versus how many simply deferred entirely to whatever the hierarchy later clarified, such that the ''victim'' population bearing interpretive uncertainty may be smaller than the stakeholder framing implies?',
    'Diary and court-record analysis of individual members'' documented decision-making in the 1890-1904 window, distinguishing those who report active uncertainty from those who report untroubled deference.',
    'If most members deferred without experiencing the ambiguity as costly, the victim-side extraction is concentrated narrowly on the subset who took the ambiguity seriously (especially the excommunicated dissenters) rather than diffused across the general membership, which would sharpen rather than widen the victim group in base_properties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(member_belief_heterogeneity, empirical, 'Whether the ambiguity''s cost was widely or narrowly distributed among ordinary members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 1890, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(marr_tr_t1897, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1897, 0.35).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1904, 0.42).
narrative_ontology:measurement(marr_tr_t1911, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1911, 0.44).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1920, 0.44).
narrative_ontology:measurement(marr_tr_t1930, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1930, 0.44).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement(marr_be_t1897, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1897, 0.44).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1904, 0.5).
narrative_ontology:measurement(marr_be_t1911, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1911, 0.53).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(marr_be_t1930, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1930, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(marr_su_t1897, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1897, 0.62).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1904, 0.6).
narrative_ontology:measurement(marr_su_t1911, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1911, 0.58).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1920, 0.57).
narrative_ontology:measurement(marr_su_t1930, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1930, 0.55).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1890, tn=1930
narrative_ontology:measurement(marr_grid_01, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(class), 1890, 0.5).
narrative_ontology:measurement(marr_grid_02, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(class), 1930, 0.6).
narrative_ontology:measurement(marr_grid_03, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(individual), 1890, 0.6).
narrative_ontology:measurement(marr_grid_04, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(individual), 1930, 0.68).
narrative_ontology:measurement(marr_grid_05, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(organizational), 1890, 0.35).
narrative_ontology:measurement(marr_grid_06, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(organizational), 1930, 0.5).
narrative_ontology:measurement(marr_grid_07, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(structural), 1890, 0.45).
narrative_ontology:measurement(marr_grid_08, marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse(structural), 1930, 0.55).
narrative_ontology:measurement(marr_grid_09, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(class), 1890, 0.4).
narrative_ontology:measurement(marr_grid_10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(class), 1930, 0.2).
narrative_ontology:measurement(marr_grid_11, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(individual), 1890, 0.55).
narrative_ontology:measurement(marr_grid_12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(individual), 1930, 0.25).
narrative_ontology:measurement(marr_grid_13, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(organizational), 1890, 0.15).
narrative_ontology:measurement(marr_grid_14, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(organizational), 1930, 0.1).
narrative_ontology:measurement(marr_grid_15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(structural), 1890, 0.2).
narrative_ontology:measurement(marr_grid_16, marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance(structural), 1930, 0.15).
narrative_ontology:measurement(marr_grid_17, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(class), 1890, 0.7).
narrative_ontology:measurement(marr_grid_18, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(class), 1930, 0.35).
narrative_ontology:measurement(marr_grid_19, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(individual), 1890, 0.85).
narrative_ontology:measurement(marr_grid_20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(individual), 1930, 0.4).
narrative_ontology:measurement(marr_grid_21, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(organizational), 1890, 0.5).
narrative_ontology:measurement(marr_grid_22, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(organizational), 1930, 0.25).
narrative_ontology:measurement(marr_grid_23, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(structural), 1890, 0.6).
narrative_ontology:measurement(marr_grid_24, marriage_commitment_legitimacy__hybrid_pragmatic_reading, stakes_inflation(structural), 1930, 0.3).
narrative_ontology:measurement(marr_grid_25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(class), 1890, 0.6).
narrative_ontology:measurement(marr_grid_26, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(class), 1930, 0.55).
narrative_ontology:measurement(marr_grid_27, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(individual), 1890, 0.75).
narrative_ontology:measurement(marr_grid_28, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(individual), 1930, 0.6).
narrative_ontology:measurement(marr_grid_29, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(organizational), 1890, 0.7).
narrative_ontology:measurement(marr_grid_30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(organizational), 1930, 0.5).
narrative_ontology:measurement(marr_grid_31, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(structural), 1890, 0.55).
narrative_ontology:measurement(marr_grid_32, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression(structural), 1930, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the marriage_commitment_legitimacy kernel. The endogenous_reinterpretation_reading treats the Manifesto as genuine, non-strategic revelation (epsilon near zero, no distinct institutional beneficiary from divine will). The exogenous_override_reading treats it as pure coerced capitulation with doctrine unchanged underneath (epsilon driven entirely by federal coercion, no institutional agency in the ambiguity itself). This hybrid_pragmatic_reading is the only one that locates both a genuine coordination function and an asymmetric extraction inside the same instrument, producing a moderate, non-zero epsilon distinct from either sibling. All three share the same underlying documentary record but are not the same constraint — each authors its own epsilon and stakeholder structure per the epsilon-invariance principle, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
