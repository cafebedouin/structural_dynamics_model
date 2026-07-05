% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Monarchy: Dual-Sourced Legitimacy (Hybrid Reading)
 *   domain: political/constitutional theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the sovereign_legitimacy kernel:
 *   the constitutional hybrid, in which ceremonial/symbolic authority is
 *   inherited (the monarchy) and political authority is delegated (elected
 *   officials), with constitutional courts and accumulated precedent
 *   mediating the boundary between them. It is not a description of 'the
 *   monarchy question' in general — the monarchical_reading (pure
 *   downward-flowing divine/traditional authority) and republican_reading
 *   (pure upward-flowing popular consent) are separate constraint stories
 *   with their own ε, beneficiaries, and victims. This story's ε is
 *   deliberately low-to-moderate: the compromise structurally reduces the
 *   extractiveness each pure form would otherwise carry (a pure monarchy's
 *   unchecked extraction from subjects; a pure republic's majoritarian
 *   override of minority symbolic/traditional attachment), but it introduces
 *   a distinct cost — ambiguity at the boundary, litigated case by case,
 *   which is where the theater_ratio drift over the interval originates.
 *
 * KEY AGENTS:
 *   - hereditary_monarch: retained ceremonial status and funding without exercised political power
 *   - elected_officials: hold actual policy authority, administer the boundary in practice
 *   - absolutist_restorationists: victims of the compromise from the monarchist pole
 *   - republican_abolitionists: victims of the compromise from the popular-sovereignty pole
 *   - constitutional_courts: the interpretive mechanism that operationalizes the settlement
 *   - general_public: diffuse beneficiary/payer, mostly passive participant in the equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.32).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.4).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Monarchy: Dual-Sourced Legitimacy (Hybrid Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political/constitutional theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '2071b8c5-e68a-49be-98f9-b3a3139d6bc7').
narrative_ontology:cs_kernel_codification('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', formalized).
narrative_ontology:cs_authority_grounding('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', lineage).
narrative_ontology:cs_interpretation_layer_present('2071b8c5-e68a-49be-98f9-b3a3139d6bc7').
narrative_ontology:cs_reading_relation('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_axiom('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', foundational, authority_is_legitimately_dual_sourced).
narrative_ontology:cs_axiom_status(authority_is_legitimately_dual_sourced, holdable).
narrative_ontology:cs_axiom_grounding('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', authority_is_legitimately_dual_sourced, conventional).
narrative_ontology:cs_axiom('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', secondary, constitutional_interpretation_mediates_rather_than_originates_authority).
narrative_ontology:cs_axiom_status(constitutional_interpretation_mediates_rather_than_originates_authority, holdable).
narrative_ontology:cs_axiom_grounding('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', constitutional_interpretation_mediates_rather_than_originates_authority, conventional).
narrative_ontology:cs_reference_frame('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', post_settlement_constitutional_compromise).
narrative_ontology:cs_drift_state('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', contemporary_constitutional_monarchy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2071b8c5-e68a-49be-98f9-b3a3139d6bc7', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_restorationists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_abolitionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, general_public).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, general_public).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, separation_of_symbolic_and_political_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial headship of state, inherited status, public funding, and a residual reserve power (dissolution consent, royal assent) that is almost never exercised against the elected government. Income and status flow from the constitutional settlement rather than from winning votes. Exit from the role is not really available — the office is the person's inherited identity — but exit from the SETTLEMENT (asserting real political power) would trigger a legitimacy crisis the monarch is structurally incentivized to avoid.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, civilizational, identity_locked, national).

% Hold and exercise actual policy-making power, legitimated through periodic elections rather than through the ceremonial apparatus. They administer the constitutional boundary in practice — deciding, through convention and statute, what remains ceremonial and what becomes political. Their exit option is bounded by electoral cycles and by the constitutional text itself, which they cannot unilaterally rewrite.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter).

% Hold that the monarch's authority should be substantive and divinely/traditionally sanctioned, not decoratively fenced off by constitutional convention. The hybrid settlement structurally forecloses their preferred arrangement: the crown they revere is real in form but hollowed of the power they believe it should hold. They have no institutional lever to reverse this without a constitutional crisis they lack the numbers to force.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_restorationists, payer,
    powerless, generational, trapped, national).

% Hold that authority should flow solely from popular consent, with no inherited component at all. They bear the cost of the compromise by having to accept continued public funding of a hereditary office, ceremonial deference in state functions, and symbolic space they consider illegitimate. They can organize and campaign for abolition, but the hybrid structure enjoys enough diffuse popular attachment that abolition rarely reaches a decisive threshold.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_abolitionists, payer,
    moderate, generational, constrained, national).

% Adjudicate disputes at the ceremonial/political boundary — whether a monarch's action was within reserve powers, whether an elected government's request for royal assent was itself constitutional. Their rulings and the accumulated body of precedent are what actually locate the boundary; text alone underdetermines it. They have no direct stake in either the crown's status or the government's policy program, but their interpretive authority is what makes the dual-source settlement operable rather than merely declared.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, observer).

% Receive institutional stability, a non-partisan ceremonial head of state, and continuity of national symbolism, while also participating in genuine electoral contestation over policy. They pay indirectly for the ceremonial apparatus (public funding, deference costs) and bear ambiguity costs when boundary disputes surface. Most treat the arrangement as background furniture rather than an active choice, which is itself part of how the settlement persists.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, general_public, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates two things that pure monarchical and pure republican settlements each fuse dangerously: the symbolic/unifying function of a head of state, and the contested, partisan business of policy-making. By routing them through different legitimation sources, neither a partisan elected official nor a single hereditary figure holds both the sword and the crown's mystique simultaneously.
% TRANSFER_FUNCTION: Moves ceremonial status, continuity, and a share of public funding to the hereditary line; moves substantive policy-making power to periodically elected officials; moves the cost of maintaining both — public funds, symbolic deference, and the recurring friction of boundary disputes — onto the general public and onto those who wanted a pure form (either fully sovereign monarchy or fully popular republic) and got neither.
% ABSENT_VOICES: Absolutist restorationists and republican abolitionists are both structurally present as declared victims but are effectively excluded from the interpretive process that maintains the boundary — constitutional courts and long-settled convention, not popular referendum, decide contested cases, and neither pure-form constituency controls that process.
% DISAPPEARANCE_RATIONALE: If the dual-source settlement vanished overnight, the polity would have to resolve immediately into one of the two pure forms (or a written republic) — the ceremonial apparatus, the reserve powers, the conventions of royal assent, and the funding arrangements are all load-bearing for a national identity and a system of checks that would otherwise need to be rebuilt from scratch, likely with real conflict over which pure form wins.
% FOUNDING_PROBLEM: Historical settlements (often post-civil-war or post-revolutionary compromises) needed to stop a fight between an entrenched hereditary claimant and an emerging popular/parliamentary power without either side achieving total victory — the hybrid was built to end an active succession or sovereignty crisis by splitting authority rather than letting one side eliminate the other.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative-government scholars (outside both the monarchy's household and the elected government) generally corroborate that the original sovereignty crisis is long resolved in most extant constitutional monarchies, and that the arrangement now persists primarily through popular sentiment, path dependency, and elite convenience rather than active crisis-prevention; the monarchy's own household communications and government constitutional-affairs offices, by contrast, continue to describe the arrangement as functionally necessary for stability, which is exactly the corroboration the sixth-question discipline treats as insufficient on its own.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32 at interval end) is low-to-moderate and rises only slowly: the hybrid removes the sharpest extraction vectors of both pure forms (arbitrary hereditary rule; unchecked majoritarian churn) but does not eliminate cost — public funding of ceremony and the opportunity cost imposed on both pure-form constituencies are real and persistent. Theater ratio climbs from 0.30 to 0.55 over the interval: as the founding sovereignty crisis recedes into history, an increasing share of the ceremonial apparatus's justification becomes performative continuity rather than active crisis-prevention, which is exactly the founding_problem_status=contested signal. Suppression declines modestly (0.50 to 0.40) as the settlement normalizes and requires less active enforcement to hold — precedent substitutes for coercion over time. Accessibility collapse (0.45) and resistance (0.50) are both mid-range, reflecting that this is a genuine compromise structure: alternatives are not fully suppressed (both pure forms remain live political projects) but the settlement's entrenchment does make displacement costly.
 *
 * PERSPECTIVAL GAP:
 *   The two beneficiary seats (monarch, elected officials) experience the arrangement as a durable, low-conflict equilibrium that lets each hold what it values without contesting the other's domain. The two victim seats experience it oppositely but for mirror-image reasons: the absolutist sees a crown stripped of its rightful substance; the republican sees an unelected office receiving public deference and funds it should never have. Constitutional courts see neither extraction nor coordination as such — they see a docket of boundary disputes to resolve by precedent, which is why the interpretive layer (not the text) is doing most of the classificatory work here.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the monarch and elected officials are declared beneficiaries because each collects something the settlement specifically preserves for them without having to defeat the other. Absolutists and republicans are declared victims not because the arrangement extracts money or labor from them in a conventional sense, but because it structurally forecloses their preferred distribution of authority while requiring their continued participation in (and partial funding of) a settlement they reject in principle. The general public sits near-symmetric: real coordination benefit (stability, non-partisan symbolism) against a real but diffuse cost (funding, ambiguity, occasional boundary-dispute friction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead specifically because the tangled_rope classification depends on there being an active coordination function, not merely inertia. If the analysis instead confirmed the founding sovereignty crisis is fully resolved everywhere the settlement persists, and public funding of ceremony continued only through path dependency with no boundary-dispute risk remaining, the classification would drift toward piton (theater without active coordination). The rising theater_ratio series is the leading indicator this story is authored to expose, without pre-deciding the verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_dispute_resolution_authority,
    'When constitutional courts resolve a ceremonial/political boundary dispute, are they discovering a pre-existing constitutional meaning or constructing new authority allocation through precedent?',
    'Compare court reasoning across multiple boundary disputes over time: if outcomes track prior precedent with high consistency and minimal drift, favor discovery; if outcomes track contemporaneous political pressure more than precedent, favor construction.',
    'If the courts are constructing rather than discovering, the mediating mechanism is itself a political authority in disguise, which would push this reading''s classification toward tangled_rope more strongly by adding a third quasi-beneficiary (the judiciary) rather than a neutral arbiter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_resolution_authority, conceptual, 'Whether constitutional interpretation is neutral arbitration or a hidden third locus of political power.').

omega_variable(
    hybrid_reading_kernel_indeterminacy,
    'Is the dual-source (hybrid) reading a genuinely stable third position, or is it a temporary equilibrium that will eventually resolve toward either the monarchical or republican pole as the founding sovereignty crisis recedes further into history?',
    'Track long-run institutional trend data: expansion or contraction of reserve powers actually exercised, changes in public funding levels, and frequency/outcome pattern of abolition or restoration referenda across multiple constitutional monarchies over multi-generational intervals.',
    'If the hybrid consistently drifts toward one pole over centuries, the kernel''s three readings are not co-equal stable options but a spectrum with two stable endpoints and one metastable midpoint — this would not change this story''s ε but would inform how the network edges to sibling readings should be weighted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_kernel_indeterminacy, empirical, 'Whether the constitutional hybrid is a stable equilibrium or a slow transition state between the two pure kernel readings.').

omega_variable(
    public_funding_natural_vs_constructed,
    'Is continued public funding of the ceremonial office a natural consequence of maintaining any head-of-state function, or is it a constructed extraction specific to the hereditary form that a non-hereditary ceremonial head (e.g., a figurehead president) would not require at comparable cost?',
    'Comparative cost analysis against republics with purely ceremonial, non-hereditary heads of state performing similar unifying functions.',
    'If comparable ceremonial-head costs are similar across hereditary and non-hereditary systems, the extractiveness attributed to the hereditary component specifically is overstated; if hereditary ceremonial heads cost substantially more for the same function, that gap is attributable extraction rather than coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_funding_natural_vs_constructed, empirical, 'Whether ceremonial funding costs are inherent to the head-of-state function or specific to hereditary succession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 80, 0.51).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 60, 0.29).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 100, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the sovereign_legitimacy kernel (constitutional_hybrid_reading, monarchical_reading, republican_reading). Each reading is authored as a separate constraint with its own ε, beneficiaries, victims, and claimed type, per the ε-invariance principle — the underlying kernel text (a claim about where legitimate authority comes from) is read three structurally distinct ways, and conflating them into one constraint with an averaged ε would violate DP-001. The constitutional_hybrid_reading occupies a low-to-moderate ε position between what would likely be a higher-ε pure monarchical reading (unchecked hereditary extraction) and a variable-ε republican reading (majoritarian risk to minority/traditional interests), because the hybrid's institutional separation dampens the sharpest extraction vector of each pure form while introducing its own boundary-dispute cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
