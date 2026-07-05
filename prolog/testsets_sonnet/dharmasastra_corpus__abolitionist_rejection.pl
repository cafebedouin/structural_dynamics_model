% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus — Abolitionist Rejection Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This story instantiates the abolitionist_rejection reading of the
 *   dharmasastra_corpus kernel: the position that the corpus's hierarchical
 *   prescriptions (particularly varna/jati ordering) are not separable from
 *   an ethical core, carry no surviving legitimate authority, and must be
 *   structurally rejected rather than reinterpreted. This is one of three
 *   sibling readings of the same kernel; the orthodox_literalist reading
 *   holds the prescriptions eternal and binding, and the reformist_contextual
 *   reading holds the ethical core separable and salvageable from the
 *   historically contingent caste prescriptions. This story does NOT
 *   adjudicate between the readings or average across them — it presents the
 *   abolitionist reading's own internally coherent structural claim: total
 *   delegitimization, victim set dissolved by dismantling the hierarchy
 *   rather than by textual reinterpretation, and beneficiary shift toward
 *   historically subordinated groups via constitutional and social
 *   mobilization rather than via revised textual authority.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: institutional beneficiary of interpretive and ritual authority
 *   - dominant_caste_landholders: powerful local agenda-setters enforcing caste boundaries through land and social sanction
 *   - dalit_communities: powerless, trapped payers bearing the corpus's historical extraction
 *   - caste_annihilation_movement: organized agenda-setter advocating total rejection over reform
 *   - constitutional_courts_and_state: institutional observer increasingly displacing textual authority with constitutional adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.86).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.8).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.86).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus — Abolitionist Rejection Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, 'ef48cfb9-4aea-47f7-b833-66cbf40eeae2').
narrative_ontology:cs_kernel_codification('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', fixed_text).
narrative_ontology:cs_authority_grounding('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', extraction).
narrative_ontology:cs_interpretation_layer_present('ef48cfb9-4aea-47f7-b833-66cbf40eeae2').
narrative_ontology:cs_reading_relation('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', foundational, hierarchy_ethics_inseparability).
narrative_ontology:cs_axiom_status(hierarchy_ethics_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', hierarchy_ethics_inseparability, conventional).
narrative_ontology:cs_axiom('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', foundational, textual_authority_fully_extinguished).
narrative_ontology:cs_axiom_status(textual_authority_fully_extinguished, holdable).
narrative_ontology:cs_axiom_grounding('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', textual_authority_fully_extinguished, deontological).
narrative_ontology:cs_reference_frame('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', revealed_dharmic_cosmic_order).
narrative_ontology:cs_drift_state('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', post_constitutional_equality_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('ef48cfb9-4aea-47f7-b833-66cbf40eeae2', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dominant_caste_landholders).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, temple_and_matha_institutions).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, lower_caste_women).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, inter_caste_couples).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, caste_annihilation_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, constitutional_equality_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically occupies the interpretive apex of the varna hierarchy the corpus codifies, controlling ritual gatekeeping, temple administration, and textual transmission. From this reading's vantage, whatever coordination function the corpus once served for this group is inseparable from the extraction it draws from subordinated castes; the group's continued invocation of the text is read as self-interested rather than authoritative.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class, beneficiary,
    institutional, civilizational, arbitrage, national).

% Use varna-derived social ranking to justify land control, labor extraction, and village-level social exclusion (untouchability practices, occupational restriction). They administer local enforcement of caste boundaries through social sanction, economic dependency, and, historically, violence — they could abandon the hierarchy without losing land, but choose not to.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dominant_caste_landholders, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, dominant_caste_landholders, agenda_setter).

% Historically assigned to the bottom of or entirely outside the varna scheme, subjected to untouchability, occupational restriction, spatial segregation, and violence justified by appeal to dharmic order. Constitutional and legal protections exist on paper but social enforcement of caste boundaries persists at the village and family level, making exit from caste identity itself largely unavailable regardless of formal law.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_communities, payer,
    powerless, civilizational, trapped, national).

% Assigned a servitor role within the fourfold scheme; bear caste-based occupational restriction and social subordination that the textual framework frames as their dharmic station. Migration to cities offers partial anonymity but caste identity resurfaces at marriage, land transactions, and local politics.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_laborers, payer,
    powerless, generational, trapped, regional).

% Bear compounded extraction from both caste and gender provisions in the corpus (restricted property rights, marriage regulation, purity codes). Sexual violence against lower-caste women has historically been under-punished partly through social structures the text is invoked to legitimate. Exit requires escaping both caste and patriarchal enforcement simultaneously.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, lower_caste_women, payer,
    powerless, generational, trapped, regional).

% Face social ostracism, family disownment, and in extreme cases honor-based violence for violating endogamy norms the corpus codifies as essential to dharmic order maintenance. State law protects their marriages formally; family and community enforcement operates independently of and often against state law.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, inter_caste_couples, payer,
    powerless, biographical, constrained, regional).

% Institutional bodies whose ritual authority, land endowments, and social standing are historically bound up with varna-ordered social organization. They administer religious life at scale and derive continuing revenue and status from practices this reading holds to be inseparable from caste hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, temple_and_matha_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Political and intellectual movement (in the lineage of Ambedkarite thought) that holds the corpus cannot be reformed or reread into legitimacy because its extractive function is constitutive, not incidental — the text's ethical core cannot be separated from its hierarchy because the hierarchy is what the ethical vocabulary was built to justify. Advocates constitutional supremacy and complete textual delegitimization rather than reinterpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, caste_annihilation_movement, agenda_setter,
    organized, civilizational, mobile, national).

% Practitioners and scholars who hold that the corpus retains legitimate authority (literal or reinterpreted) would object strenuously to this reading's total rejection, but within THIS constraint's frame their position is not adjudicated — it is a sibling reading (orthodox_literalist, reformist_contextual), not a voice inside this constraint's own operation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, orthodox_and_reformist_readers, excluded,
    organized, civilizational, constrained, national).

% Adjudicate disputes where personal-law claims grounded in religious textual authority collide with constitutional equality guarantees. Increasingly treat caste-based discrimination claims under constitutional and statutory anti-discrimination frameworks rather than under the textual corpus itself, which this reading treats as evidence the corpus has lost adjudicative standing.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, constitutional_courts_and_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, diffuse).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading. Where orthodox and reformist readings claim the corpus solves a genuine coordination problem (social role stability, ethical guidance, ritual order), the abolitionist reading holds that the apparent coordination function is itself the extraction mechanism — hierarchy dressed as order. No residual coordination benefit survives dismantling; the reading asserts the corpus never solved a problem for the parties it burdened, only for the parties it privileged.
% TRANSFER_FUNCTION: Moves social status, land access, ritual authority, marriage-market position, and physical security from lower-caste and Dalit communities to upper-caste and priestly groups, mediated through textual claims of dharmic legitimacy that this reading holds have no independent moral standing.
% ABSENT_VOICES: Orthodox and reformist readers who hold the text retains authority are not adjudicated within this constraint — they are structurally excluded from this reading's own frame by design (the reading's core premise is that no defensible interpretive position remains). Their objections belong to the sibling readings, not to this one.
% DISAPPEARANCE_RATIONALE: Under this reading, if the corpus's remaining social authority disappeared overnight, caste-based land control, ritual gatekeeping, and endogamy enforcement would lose their primary legitimating vocabulary — enforcement would have to rest on naked social power alone, without the cover of dharmic sanction, which the reading holds would accelerate erosion of caste practice already underway through constitutional law, urbanization, and inter-caste mobilization.
% FOUNDING_PROBLEM: The corpus was originally compiled (per orthodox and reformist accounts) to codify social duty, ritual order, and dispute resolution across a stratified agrarian society. This reading holds that the actual founding function was to stabilize and naturalize a specific extraction hierarchy (varna/jati) by dressing it in the vocabulary of cosmic and moral order.
% FOUNDING_PROBLEM_CORROBORATION: Ambedkarite scholarship, Dalit testimonial literature, and comparative historical sociology (outside the priestly and landholding beneficiary groups) attest that whatever social-ordering function the corpus served has been superseded by constitutional equality law and modern state administration, while its hierarchy-legitimating function persists independently and is what abolitionist scholarship targets. Orthodox and reformist proponents (inside or adjacent to the beneficiary groups) dispute this status entirely — that dispute is the kernel contest itself, recorded structurally rather than adjudicated here.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) and suppression high (0.8) because this reading holds the corpus's hierarchy-legitimating function to be its primary operative content, not an incidental byproduct of a genuine coordination function — there is no coordination benefit to net against the extraction. Accessibility_collapse is authored moderate (0.35), not high, because under this reading alternatives (constitutional law, secular ethics, competing religious traditions) are increasingly available and used, even though social enforcement still constrains exit for many. Resistance is authored high (0.88) reflecting both the historical resistance of caste-oppressed communities and the resistance the abolitionist position itself meets from orthodox and reformist defenders of the text. The temporal series shows suppression_requirement declining through the mid-interval (constitutional protections, urbanization, legal reform) before rising again toward the end — reflecting documented backlash dynamics and re-assertion of caste enforcement in some regions even as formal legal suppression recedes.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (priestly class, landholders, temple institutions), the corpus is experienced as a stable ethical and social order worth defending in some form (this is precisely the orthodox and reformist sibling readings' position, excluded from adjudication here). From the payer seats, the identical historical structure computes as an actively enforced extraction mechanism whose legitimating vocabulary has outlived any defensible authority. This story deliberately does not reconcile these; it authors the abolitionist reading's own structural facts and lets the engine compute the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Priestly, landholding, and institutional religious beneficiaries sit near the full-beneficiary end of directionality: they collect status, land access, and ritual authority through the hierarchy the corpus encodes, with arbitrage-grade exit (they can abandon the framework without losing accumulated position). Dalit communities, shudra laborers, lower-caste women, and inter-caste couples sit near the full-target end: trapped or constrained exit, generational to civilizational time horizon of harm, and no meaningful ability to exit caste identity itself even where formal legal exit exists. The caste_annihilation_movement is authored as agenda_setter rather than payer because, within this reading, the movement is not merely enduring the constraint but actively contesting and seeking to dismantle its legitimating structure — it sets the counter-agenda.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves what would otherwise be mandatrophy analysis by refusing the reform frame entirely: rather than asking whether the corpus's founding function has become obsolete while some institutional shell persists (the piton question), the abolitionist reading holds the founding function itself — hierarchy legitimation — was never a coordination function to begin with, so there is no mandate to declare obsolete, only an extraction structure to reject. The founding_problem_status is authored 'dead' specifically because constitutional equality law and social mobilization are held to have superseded whatever residual function survived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_inseparability_claim,
    'Is the ethical vocabulary of dharma (righteous conduct, duty) actually constitutively bound to the varna/jati hierarchy, as this reading claims, or is it separable as the reformist_contextual reading claims?',
    'Close textual-historical analysis of whether pre-varna or non-hierarchical formulations of dharma exist within the broader corpus and its antecedents, and whether communities have successfully practiced a de-hierarchized dharma ethic without reintroducing caste ordering.',
    'If separable, the reformist reading''s more moderate reclassification (retained partial authority, declining ε) is the more structurally accurate account and this reading''s total-rejection premise overstates inseparability. If inseparable, this reading''s total rejection is the structurally correct response and reformist reinterpretation is itself a cover story preserving residual extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_inseparability_claim, conceptual, 'Whether dharma''s ethical core is separable from caste hierarchy or constitutively bound to it — the central kernel-level dispute.').

omega_variable(
    victim_set_dissolution_completeness,
    'Does dismantling the textual framework''s authority actually dissolve the victim set (as this reading''s structural delta claims), or does caste-based extraction persist through purely social/economic mechanisms independent of textual legitimation, meaning the victim set survives even total textual delegitimization?',
    'Longitudinal sociological data on caste-based discrimination and violence in populations where textual religious authority has been explicitly and publicly rejected (e.g., Ambedkarite Buddhist conversion communities) compared to populations retaining nominal textual adherence.',
    'If caste extraction persists after textual rejection, the corpus was never the primary extraction mechanism and this reading over-attributes causal power to text versus social structure. If extraction substantially recedes, the reading''s causal claim about the corpus''s centrality is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_dissolution_completeness, empirical, 'Whether textual delegitimization alone dissolves caste extraction or whether extraction has an independent social-structural life.').

omega_variable(
    beneficiary_reclassification_ambiguity,
    'Does this reading''s claimed beneficiary shift to formerly oppressed groups describe an already-achieved structural fact, an emerging trend, or an aspirational political claim not yet realized in most communities?',
    'Disaggregated data on land ownership, political representation, and social mobility outcomes for Dalit and lower-caste communities pre- and post- major legal/constitutional interventions, by region.',
    'If the shift is substantially achieved, the reading''s beneficiary declarations in base_properties should weight toward formerly oppressed groups more heavily than authored. If aspirational, the current beneficiary/victim structure authored here (priestly/landholding beneficiaries, Dalit/lower-caste victims) remains the operative present-tense structure, which is how this story authors it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_reclassification_ambiguity, empirical, 'Whether the beneficiary shift claimed by this reading is achieved fact or aspirational political horizon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.05).
narrative_ontology:measurement(dhar_tr_t15, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 15, 0.08).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 30, 0.12).
narrative_ontology:measurement(dhar_tr_t45, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 45, 0.15).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 60, 0.14).
narrative_ontology:measurement(dhar_tr_t75, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 75, 0.15).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.92).
narrative_ontology:measurement(dhar_be_t15, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 15, 0.9).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(dhar_be_t45, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 45, 0.8).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(dhar_be_t75, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 75, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(dhar_su_t15, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(dhar_su_t45, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(dhar_su_t75, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 75, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% Part of the dharmasastra_corpus kernel family (3 readings). orthodox_literalist claims full retained authority and treats hierarchy as legitimate coordination (lowest authored ε among the three). reformist_contextual claims partial retained authority with a separable, salvageable ethical core (moderate, declining ε as reinterpretation proceeds). abolitionist_rejection (this story) claims zero retained authority and inseparability of ethical core from hierarchy (highest, persistently high ε). Each story carries its own stable ε and its own beneficiary/victim structure per the ε-invariance principle; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
