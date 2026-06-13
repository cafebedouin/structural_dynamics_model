% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor-Violence Legitimacy Constraint (Contraction Reading)
 *   domain: social/legal/commitment_system
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the honor-violence
 *   legitimacy kernel. The contraction reading asserts that dueling became
 *   structurally unthinkable because the concept of honor itself was actively
 *   redefined to exclude violence as a legitimate response. The state legal
 *   apparatus and reformer intellectual coalition worked to anchor honor in
 *   civic virtue, rational reputation, and legal vindication rather than
 *   martial valor and willingness to kill. This was not merely a cost/benefit
 *   shift (the DROP reading) or a coincidence of both forces (the COMPOSITE
 *   reading) — it was a deliberate conceptual reconstruction that moved
 *   violence outside the boundary of legitimate honor. Under this reading, a
 *   duelist in 1850 faces not just legal penalties but a changed meaning of
 *   honor itself: continuing to duel marks you not as honorable but as
 *   barbaric, irrational, and outside civilization. This reading treats the
 *   constraint as substantially extractive because the redefinition transfers
 *   authority over honor from distributed actors (individuals, military
 *   culture, regional systems) to centralized state and intellectual
 *   authority.
 *
 * KEY AGENTS:
 *   - hereditary_nobility_dueling_practitioners: holders of the old honor system; experience dueling's delegitimization as a loss of identity; identity_locked exit
 *   - state_legal_monopoly_holders: institutional agenda-setters who redefine honor and enforce the new definition through prosecution and institutional building
 *   - emerging_middle_class_peaceful_honor: beneficiaries who gain status access through a non-violent honor system; mobile exit options
 *   - reformer_intellectual_coalition: organized beneficiaries who articulate the new honor definition and claim epistemic authority over what honor truly is
 *   - honor_bound_military_officers: constrained payers caught between state authority and military culture; partly excluded from the redefinition conversation
 *   - legal_court_institutions: institutional beneficiaries that expand in authority and resource as honor disputes are routed through them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.62).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.71).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor-Violence Legitimacy Constraint (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "social/legal/commitment_system").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '4334b0c9-5d5c-45c6-8770-7a50e1235e34').
narrative_ontology:cs_kernel_codification('4334b0c9-5d5c-45c6-8770-7a50e1235e34', fixed_text).
narrative_ontology:cs_authority_grounding('4334b0c9-5d5c-45c6-8770-7a50e1235e34', lineage).
narrative_ontology:cs_interpretation_layer_present('4334b0c9-5d5c-45c6-8770-7a50e1235e34').
narrative_ontology:cs_reading_relation('4334b0c9-5d5c-45c6-8770-7a50e1235e34', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('4334b0c9-5d5c-45c6-8770-7a50e1235e34', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('4334b0c9-5d5c-45c6-8770-7a50e1235e34', foundational, honor_is_semantically_malleable).
narrative_ontology:cs_axiom_status(honor_is_semantically_malleable, holdable).
narrative_ontology:cs_axiom_grounding('4334b0c9-5d5c-45c6-8770-7a50e1235e34', honor_is_semantically_malleable, conventional).
narrative_ontology:cs_axiom('4334b0c9-5d5c-45c6-8770-7a50e1235e34', foundational, violence_exclusion_is_core_to_modern_honor).
narrative_ontology:cs_axiom_status(violence_exclusion_is_core_to_modern_honor, holdable).
narrative_ontology:cs_axiom_grounding('4334b0c9-5d5c-45c6-8770-7a50e1235e34', violence_exclusion_is_core_to_modern_honor, deontological).
narrative_ontology:cs_reference_frame('4334b0c9-5d5c-45c6-8770-7a50e1235e34', enlightenment_rationalist_honor).
narrative_ontology:cs_drift_state('4334b0c9-5d5c-45c6-8770-7a50e1235e34', contemporary_late_19th_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4334b0c9-5d5c-45c6-8770-7a50e1235e34', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_monopoly_holders).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, emerging_middle_class_peaceful_honor).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_security_apparatus).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, hereditary_nobility_dueling_practitioners).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, honor_bound_military_officers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, reformer_intellectual_coalition).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, legal_court_institutions).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_violence_doctrine).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, rationalist_reformer_epistemic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% For centuries, a duel was the only honorable response to insult. The redefinition of honor to exclude violence removes dueling from the legitimate response set without offering an equivalent mechanism. A nobleman faces the choice: accept the new definition and absorb the insult as honorable silence (a conceptual inversion they experience as loss of status), or continue dueling and face criminal prosecution as a murderer. Their identity as honor-bound actors is fused with the structural position dueling occupied — exit means reconstructing honor itself.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, hereditary_nobility_dueling_practitioners, payer,
    powerful, biographical, identity_locked, national).

% State legal apparatus claims the exclusive right to legitimate violence and defines honor in terms compatible with that monopoly. Redefines honor as rational, civic, non-violent — channeled through legal remedies (suits for slander, reputation recovery through law courts). Enforces the new definition through criminal prosecution of duelists, codifying dueling as murder regardless of consent. Builds institutions (courts, reputation mechanisms) that carry honor functions but route through state machinery.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_monopoly_holders, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from a redefinition of honor that does not require willingness to kill or die. Honors civic virtue, intellectual reputation, professional standing, legal vindication. The new honor system is accessible without the martial training, wealth, and lethal risk of the dueling economy. They have mobility — can enter, thrive in, and exit this honor system without identity fusion.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, emerging_middle_class_peaceful_honor, beneficiary,
    organized, generational, mobile, national).

% Military culture maintained dueling as a legitimate officer honor mechanism well into the 19th century. The redefinition creates tension: officers are subject to state authority (cannot openly duel) but positioned within institutions that still valorize martial honor and personal valor. They are partly excluded from the conversation about the new honor definition — military institutional culture has not been consulted — and face suppressed dueling (underground or abroad) rather than open legitimate practice.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_bound_military_officers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, honor_bound_military_officers, excluded).

% Intellectuals, legal theorists, and Enlightenment rationalists actively champion the redefinition of honor away from violence. They articulate why dueling is irrational, primitive, and destructive to social order. They offer an alternative honor system grounded in reputation, reason, and legal standing. Their authority comes from claiming to know what honor truly is, and they actively reshape institutions and discourse to embed the new definition.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, reformer_intellectual_coalition, beneficiary,
    organized, biographical, mobile, national).

% Benefits from the elimination of private violence and the transfer of all legitimate force to state monopoly. A dueling culture allows aristocrats to settle disputes through violence outside state control; the redefinition consolidates state security authority. The security apparatus does not operate the honor redefinition directly but is a primary beneficiary of the extraction it produces.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_security_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Courts become the new mechanism for honor recovery: slander suits, defamation proceedings, reputation restoration through legal judgment. The institutions themselves expand in authority and resource allocation as honor disputes are rerouted through them. They actively enforce the redefinition by prosecuting duelers and offering legal alternatives.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_court_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, legal_court_institutions, beneficiary).

% Subsocieties and subcultures that maintain alternative honor systems (street codes, working-class vendetta cultures, regional practices) are structurally excluded from the conversation about the new honor definition. The definition is imposed top-down by state and intellectual authority, not negotiated across honor systems. Their exclusion is enforced through law and social stigma.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, rival_honor_systems, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, state_legal_monopoly_holders).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transfers authority over legitimate violence from distributed personal-honor mechanisms to centralized state legal monopoly. Solves the coordination problem of how to resolve disputes without allowing private individuals to settle them through force. Creates a unified honor system compatible with state security and rational law.
% TRANSFER_FUNCTION: Moves the right to legitimate violent response from honor-bound individuals and their seconds to the state legal apparatus. Simultaneously moves the definition of honor itself from military-aristocratic (personal valor, martial courage) to civic-legal (rational standing, legal vindication, reputation before law). The losers are dueling practitioners and hereditary honor systems; the gainers are state monopoly holders, middle-class professionals, and reformer intellectuals.
% ABSENT_VOICES: Military officer cultures (consulted only late or not at all in the process), working-class honor systems and vendetta cultures (structurally excluded from the definition-making process), regional practices and subsocieties maintaining alternative honor systems (overridden by top-down redefinition). Women, who were excluded from both dueling and the new honor system's authority centers, had no seat in the redefinition negotiation.
% DISAPPEARANCE_RATIONALE: If the redefinition of honor to exclude violence vanished and dueling returned to structural legitimacy, the entire edifice of state legal monopoly on violence would be compromised. Aristocrats would settle disputes privately again, military officers would openly duel, and state authority would have to reassert monopoly through renewed enforcement. The redefinition is not a thin rule but a fundamental shift in how legitimate action is conceptualized — its disappearance would require reconstructing that entire authorization system.
% FOUNDING_PROBLEM: Private violence (dueling) was consuming aristocratic lives, destabilizing families, disrupting state order, and operating outside state legal authority. Duels could not be easily criminalized without directly confronting the honor system that legitimated them. The founding problem was: how to eliminate private dueling without delegitimizing honor itself, which is central to aristocratic identity and military culture.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and reformer intellectuals attest the problem was genuine and is solved by the redefinition. Historical evidence from dueling statistics, state legislation, and institutional records supports that dueling rates declined sharply after the redefinition was actively enforced. However, the DROP reading attests that external costs (professionalization of armies, rise of firearms making dueling more lethal and less fair, economic disruption) did more causal work than the redefinition itself. Historians outside the state authority structure are divided on whether the founding problem was 'dueling is private violence' or 'dueling is becoming economically and militarily obsolete'; the former narration justifies the redefinition as a solution, the latter suggests the redefinition captured and codified a shift already underway.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint shows sharp extraction growth from 1600 to 1830, plateauing at 1870-1900. This trajectory tracks the institutionalization of the honor redefinition: early 1600s, dueling was structurally legitimate and no suppression was needed (low extractiveness). By 1700, state authorities began active prosecution and intellectual attacks on dueling's legitimacy (extractiveness rises). The 1775-1830 window marks peak institutional conflict and active redefinition — state law, intellectual writings, and institutional building (court expansion, professional reputation mechanisms) are all in play. By 1870-1900, the redefinition is embedded: dueling is criminal, courts handle honor disputes, and the new honor system is hegemonic. Extractiveness stabilizes because the constraint's work is done — the old honor system is defeated and the new one is normalized. Theater rises sharply 1775-1870 (the redefinition process itself is theatrical: manifestos, legislation, prosecutions, institutional displays) and then stabilizes at moderate levels (maintenance theater: courts still display honor, law still invokes honor, but the core conflict is resolved). Suppression requirement rises in lockstep with institutional enforcement machinery. One shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, this is straightforward coordination with some opposition to suppress. From the hereditary nobility seat, this is the systematic destruction of a way of life dressed in the language of enlightenment and rationality. From the military officer seat, there is tension: you are subject to state authority but embedded in a culture that still honors the very thing the state is criminalizing. From the middle-class seat, this is expansion of possibility and access. The measurement series captures this divergence in the rising theater_ratio: early institutional theater (manifestos, new court procedures, public prosecutions) gradually becomes normalized so deeply that the theatrical character is hidden (courts are just courts, not displays of the new honor order). At 1900, theater is still 0.44 because the old honor system's defenders have never fully capitulated — dueling persists in military culture and aristocratic subcultures, but it is driven underground, mythologized, and marked as primitive/barbaric in official discourse. The theater is the gap between the official story (dueling is gone, honor is civilized) and the structural reality (it persists in suppressed form in certain communities).
 *
 * DIRECTIONALITY LOGIC:
 *   State legal monopoly holders are the structural beneficiary (d near 0.0): they gain exclusive authority over violence legitimacy, expand institutional reach, and face no suppression. Reformer intellectuals are beneficiaries (d ~0.1): they gain epistemic authority and institutional platforms, with mobile exit (they can abandon the cause but lose platform, not identity). Emerging middle class peaceful honor are beneficiaries (d ~0.15): they gain honor access at lower cost and risk; their exit is mobile — they were not locked into the old system. Hereditary nobility and military officers are targets (d ~0.85 and 0.75 respectively): they lose structural legitimacy, face criminal prosecution (hereditary nobility especially), and are identity-locked — exiting means reconstructing what honor means to them. The identity_locked exit for nobility is critical: they cannot simply opt out; continuing to honor their own identity requires resisting the state redefinition, which makes them criminals. Rival honor systems are trapped and excluded (d ~0.9): structurally barred from the conversation and overridden by top-down definition.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy. The founding problem was 'eliminate private dueling without delegitimizing honor itself.' The contraction reading asserts the solution worked: honor is redefined to exclude violence, dueling becomes structurally unthinkable (not just practically rare), and the state monopoly on violence is consolidated. However, the DROP reading asserts the founding problem is actually 'private dueling had become practically obsolete due to external costs and evolution of warfare.' Under DROP, the constraint's mandate (redefine honor) was never actually necessary — dueling rates fell for reasons orthogonal to the redefinition. The COMPOSITE reading suggests both readings have truth: the redefinition accelerated a shift that was already underway. This is a classic mandatrophy scenario: the constraint's stated mandate (redefine honor to eliminate dueling) may have been delivered after the problem (private dueling) had already begun to solve itself through other forces. The constraint's persistence in the formal legal and institutional realm (courts, laws, official definition) may be inertial performance rather than functional necessity. This ambiguity is routed to the omega variables, not resolved in the base classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    redefinition_vs_obsolescence_boundary,
    'Did the honor redefinition actively CAUSE dueling to become structurally unthinkable, or did external costs (professionalization of armies, rise of firearms, economic disruption) make dueling practically obsolete first, and the redefinition merely codified what was already happening?',
    'Comparative historical analysis: examine jurisdictions where the redefinition was aggressively enforced vs. those where it was adopted slowly or not at all. If dueling rates fell equally in both, the redefinition was secondary. If dueling persisted longer in low-enforcement jurisdictions, the redefinition was active. Examine military records to see if dueling persisted or declined independently of civil legal redefinition.',
    'If redefinition was active: the constraint is genuinely extractive (state and intellectual class captured authority over honor). If obsolescence was primary: the constraint is inertial performance (codifying a shift already underway) — reclassify toward piton. If both operated: intermediate extraction, moderate theater (constraint performs both coordination and capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redefinition_vs_obsolescence_boundary, empirical, 'Whether the honor redefinition caused dueling''s decline or codified an already-occurring obsolescence.').

omega_variable(
    identity_locked_suppression_mechanism,
    'Is the suppression experienced by hereditary nobility structural (legal penalty, institutional exclusion, social stigma) or internalized (they have absorbed the new honor definition and now believe dueling is irrational, and the suppression persists even when legal enforcement is absent)?',
    'Post-legal-reform trajectory: if duelists continue in communities where enforcement is lax but the redefinition is hegemonic, suppression is partially internalized. If duelists resume when enforcement lapses, suppression is primarily structural. Examine autobiographical and literary evidence from the period to see whether duelists frame their choice as internalized shame or external constraint.',
    'If internalized: the constraint''s effective suppression is higher than the raw metric suggests; the target carries the suppression with them after legal enforcement stops. Reclassifies toward stronger snare. If structural: suppression depends on active enforcement; degradation of enforcement reduces it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression_mechanism, empirical, 'Whether suppression is structural (external enforcement) or internalized (belief change).').

omega_variable(
    kernel_reading_commission_authority,
    'This story instantiates the CONTRACTION reading of the honor-violence legitimacy kernel. But what was the primary authority that legitimated the new honor definition? Was it the state legal apparatus (lineage grounding: continuity with enlightenment rationalism), or was it intellectual/epistemic authority (experts declaring what honor really is), or was it pragmatic authority (honor redefinition emerged from observed benefits of the new system)?',
    'Examine the primary justifications given by state authorities, reformer intellectuals, and legal practitioners for why dueling became illegitimate. If the core justification is ''dueling is outside state monopoly,'' authority grounding is extraction. If the core justification is ''dueling is irrational and primitive,'' authority grounding is expertise/epistemic. If the core justification is ''honor is better served by legal vindication than by violence,'' authority grounding is practice/pragmatic.',
    'Authority grounding shifts the lens on whether the constraint represents legitimate institutional evolution (lineage, practice) or capture (extraction). Affects which reading of the kernel (CONTRACTION, DROP, COMPOSITE) is most defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_commission_authority, conceptual, 'What authority grounded the honor redefinition and legitimated the new honor definition.').

omega_variable(
    sibling_reading_empirical_divergence,
    'The DROP reading claims dueling became rare due to external costs, not redefinition. The empirical test is straightforward: what does the historical record show about the TIMING of decline? Did dueling rates begin falling before the redefinition was active (supporting DROP) or only after (supporting CONTRACTION)?',
    'Quantitative historical analysis: compile dueling-rate time series from court records, military records, and contemporary accounts. Identify the inflection point (where decline accelerates). If the inflection precedes major prosecutions and legislative action, DROP is better supported. If the inflection coincides with the redefinition campaign, CONTRACTION is better supported. If the inflection predates and the redefinition accelerates further decline, COMPOSITE is better supported.',
    'This is the core empirical ground that separates the three readings. The outcome determines which reading has the strongest claim to be THE accurate history of dueling''s decline, and whether the constraint''s mandate (redefine honor) was necessary or inertial performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_divergence, empirical, 'Timing relationship between dueling decline and the honor redefinition campaign.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_violence_legitimacy__contraction_reading, theater_ratio, 1600, 0.05).
narrative_ontology:measurement_basis(hono_tr_t1600, observed).
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1775, honor_violence_legitimacy__contraction_reading, theater_ratio, 1775, 0.15).
narrative_ontology:measurement_basis(hono_tr_t1775, observed).
narrative_ontology:measurement(hono_tr_t1830, honor_violence_legitimacy__contraction_reading, theater_ratio, 1830, 0.28).
narrative_ontology:measurement_basis(hono_tr_t1830, observed).
narrative_ontology:measurement(hono_tr_t1870, honor_violence_legitimacy__contraction_reading, theater_ratio, 1870, 0.42).
narrative_ontology:measurement_basis(hono_tr_t1870, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.44).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement_basis(hono_be_t1600, observed).
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.32).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1775, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1775, 0.51).
narrative_ontology:measurement_basis(hono_be_t1775, observed).
narrative_ontology:measurement(hono_be_t1830, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1830, 0.62).
narrative_ontology:measurement_basis(hono_be_t1830, observed).
narrative_ontology:measurement(hono_be_t1870, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1870, 0.68).
narrative_ontology:measurement_basis(hono_be_t1870, observed).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement_basis(hono_su_t1600, observed).
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.28).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1775, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1775, 0.45).
narrative_ontology:measurement_basis(hono_su_t1775, observed).
narrative_ontology:measurement(hono_su_t1830, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1830, 0.62).
narrative_ontology:measurement_basis(hono_su_t1830, observed).
narrative_ontology:measurement(hono_su_t1870, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1870, 0.71).
narrative_ontology:measurement_basis(hono_su_t1870, observed).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement_basis(hono_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, state_monopoly_violence_doctrine).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, aristocratic_military_identity_erosion).

% DUAL FORMULATION NOTE:
% The honor-violence legitimacy kernel has three readings: CONTRACTION (this story: dueling became structurally unthinkable via redefinition), DROP (dueling became practically rare via external costs), COMPOSITE (both forces operated). These are three separate constraint stories with different ε values and beneficiary structures. The CONTRACTION reading asserts higher extraction because the redefinition is an active transfer of authority from many to few. The DROP reading would show lower extractiveness because the decline is presented as functional obsolescence, not capture. The COMPOSITE reading would show moderate extractiveness and high theater (the constraint performs a redefinition but is partly driven by external forces it does not acknowledge). All three stories must be linked via network.affects_constraints because understanding one reading requires understanding why it diverges from the siblings. No single history of dueling can be told without accounting for the interpretive choice: what CAUSED the decline?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
