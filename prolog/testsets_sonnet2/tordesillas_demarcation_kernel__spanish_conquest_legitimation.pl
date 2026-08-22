% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Tordesillas Line as Papal License for Spanish Conquest and Indigenous Subjugation
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This story authors the SPANISH reading of the Tordesillas demarcation
 *   kernel: the 1493-1494 papal bulls and treaty as a license for the Spanish
 *   Crown to conquer, claim sovereignty over, and extract labor and wealth
 *   from territories and peoples west of the demarcation line, with
 *   evangelization serving as the nominal consideration. This is distinct
 *   from the Portuguese reading of the SAME kernel (exploration-rights
 *   confirmation and rival-exclusion east of the line), which is authored as
 *   a separate constraint with its own extraction profile — the ε-invariance
 *   principle requires this because the two readings have entirely different
 *   victim sets, different beneficiary structures, and different extraction
 *   magnitudes even though both cite the identical treaty text. The
 *   encomienda system, the Requerimiento ritual, and the demographic collapse
 *   of indigenous populations under forced labor and disease are the concrete
 *   extraction mechanisms this reading traces.
 *
 * KEY AGENTS:
 *   - spanish_crown: agenda_setter, institutional power, global scope - issues title and authorizes conquest
 *   - spanish_colonial_administration: agenda_setter/beneficiary - implements extraction on the ground
 *   - encomenderos: beneficiary, powerful, mobile exit - direct recipients of coerced labor
 *   - catholic_missionary_orders: beneficiary/observer, split internally between complicity and dissent
 *   - indigenous_populations_west_of_line: payer, powerless, trapped - primary victim class
 *   - enslaved_and_coerced_laborers_of_encomienda: payer, powerless, trapped - direct labor extraction targets
 *   - indigenous_polities_dispossessed_of_sovereignty: payer/excluded - juridically erased political structures
 *   - portuguese_crown: excluded from this reading's benefit stream, party only to the rival-exclusion function
 *   - bartolome_de_las_casas_and_dissenting_clergy: excluded, moderate power, documented the extraction from partially outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.93).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.9).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.93).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Tordesillas Line as Papal License for Spanish Conquest and Indigenous Subjugation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '1711542f-a6a5-4953-bb51-d024e0d02d41').
narrative_ontology:cs_kernel_codification('1711542f-a6a5-4953-bb51-d024e0d02d41', formalized).
narrative_ontology:cs_authority_grounding('1711542f-a6a5-4953-bb51-d024e0d02d41', lineage).
narrative_ontology:cs_interpretation_layer_present('1711542f-a6a5-4953-bb51-d024e0d02d41').
narrative_ontology:cs_reading_relation('1711542f-a6a5-4953-bb51-d024e0d02d41', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('1711542f-a6a5-4953-bb51-d024e0d02d41', foundational, papal_grant_confers_dominium_over_inhabited_lands).
narrative_ontology:cs_axiom_status(papal_grant_confers_dominium_over_inhabited_lands, holdable).
narrative_ontology:cs_axiom_grounding('1711542f-a6a5-4953-bb51-d024e0d02d41', papal_grant_confers_dominium_over_inhabited_lands, theological).
narrative_ontology:cs_axiom('1711542f-a6a5-4953-bb51-d024e0d02d41', foundational, evangelization_mandate_justifies_coercive_subjugation).
narrative_ontology:cs_axiom_status(evangelization_mandate_justifies_coercive_subjugation, overridden).
narrative_ontology:cs_axiom_grounding('1711542f-a6a5-4953-bb51-d024e0d02d41', evangelization_mandate_justifies_coercive_subjugation, theological).
narrative_ontology:cs_reference_frame('1711542f-a6a5-4953-bb51-d024e0d02d41', papal_plenitudo_potestatis_temporal_grant).
narrative_ontology:cs_drift_state('1711542f-a6a5-4953-bb51-d024e0d02d41', post_valladolid_debate_and_school_of_salamanca, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('1711542f-a6a5-4953-bb51-d024e0d02d41', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enslaved_and_coerced_laborers_of_encomienda).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_polities_dispossessed_of_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petitioned Rome for the papal bulls (Inter Caetera, later formalized at Tordesillas) that granted title to lands west of the demarcation line contingent on evangelization. Uses the grant to authorize conquest expeditions, issue encomienda charters, and claim legal title superior to any indigenous polity or rival European crown. Collects tribute, labor, and precious metals extracted through the resulting colonial administration.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% Viceroys, audiencias, and conquistador-administrators implement the grant on the ground: issuing encomienda titles, adjudicating disputes over labor allocation, and enforcing tribute collection. They administer the extraction directly and are structurally positioned to alter its terms, but their institutional survival depends on the extraction continuing.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, beneficiary).

% Conquistadors and settlers granted encomienda rights over specific indigenous populations, entitling them to unpaid or coerced labor and tribute in exchange for nominal 'protection' and Christian instruction. They can relocate, expand holdings, or return to Spain wealthy; their claim to the labor and land rests entirely on the papal-derived title chain.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos, beneficiary,
    powerful, biographical, mobile, regional).

% Franciscans, Dominicans, and Jesuits are granted the evangelization mandate that is the papal grant's stated justification. Some orders (notably Dominican friars like Montesinos and Las Casas) turned this mandate into the earliest documented critique of the encomienda's brutality, while others directly benefited from mission labor systems that mirrored encomienda extraction.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders, beneficiary,
    organized, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders, observer).

% Entire polities and peoples across the Americas found their lands, labor, and bodies claimed under a legal title issued by an authority (the Pope) they had no part in constituting and no standing to contest. Resistance was met with military conquest; conversion was frequently coerced; population collapse followed from forced labor, disease, and violence. There is no exit from a claim asserted over the totality of the territory.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Individuals and communities assigned to specific encomenderos performed forced labor in mines, plantations, and households under threat of violence. The 'protection and instruction' owed in exchange was rarely honored; mortality from overwork, mistreatment, and disease was extreme. Flight was punished as rebellion against a legally sanctioned arrangement.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enslaved_and_coerced_laborers_of_encomienda, payer,
    powerless, biographical, trapped, regional).

% Existing political structures — empires, confederacies, chiefdoms — were juridically erased by a title chain that treated their territories as available for papal grant regardless of prior occupation or governance. Their sovereignty claims had no forum in the legal system that dispossessed them; the Requerimiento (read, often untranslated, before attacks) formalized this exclusion from the conversation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_polities_dispossessed_of_sovereignty, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_polities_dispossessed_of_sovereignty, excluded).

% Party to the same kernel but reading it for the opposite hemisphere: recognized the line's authority to exclude Spain from the eastern zone while Spain used the identical instrument to claim the west. Portugal's interest in this reading is limited to the line's exclusionary function against Castile, not the treatment of peoples west of it.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, excluded,
    institutional, generational, constrained, global).

% Documented the encomienda's atrocities and argued before the Crown and Church that the conquest violated the evangelization mandate the grant was supposedly premised on. Their objections produced reforms (the New Laws of 1542) that were resisted and partially reversed by encomendero interests; their voice was heard but structurally overridden by the beneficiaries who administered enforcement.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, bartolome_de_las_casas_and_dissenting_clergy, excluded,
    moderate, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Among European Christian monarchies, the papal grant solved a genuine (for them) coordination problem: preventing armed conflict between Spain and Portugal over overlapping claims to newly encountered lands, by having a mutually recognized authority (the Papacy) arbitrate a demarcation line both crowns would honor.
% TRANSFER_FUNCTION: The grant moves land title, labor, tribute, and mineral wealth from indigenous populations and polities west of the line to the Spanish Crown, colonial administrators, and encomenderos, using evangelization as the stated consideration for the transfer.
% ABSENT_VOICES: No indigenous polity, ruler, or representative was party to the negotiation between Spain, Portugal, and Rome. The Requerimiento's formal 'offer' of submission was read in Spanish or Latin to peoples who could not understand it, making the entire adjudicating framework one from which the payer population was categorically excluded by design.
% DISAPPEARANCE_RATIONALE: Without the papal grant's legal cover, the Spanish Crown's territorial claims and the encomienda's legal basis lose their founding title-chain; conquest and extraction would have required a different (likely purely military-conquest, no religious-legal veneer) justification, altering both the pace of colonization and the legal architecture used to defend it in later disputes such as the Valladolid debate.
% FOUNDING_PROBLEM: European monarchs needed a mechanism to avoid war with each other over new territorial claims, and Spain specifically needed a legal-theological justification for asserting title over inhabited lands and subjugating their populations under a Christian framework that in principle prohibited enslaving free peoples without just cause.
% FOUNDING_PROBLEM_CORROBORATION: The inter-crown war-avoidance problem was corroborated as resolved by both crowns' continued adherence to the line for decades; the indigenous-subjugation problem's 'solution' (evangelization as just cause) was disputed from within the Church itself — Dominican friars including Antonio de Montesinos and Bartolome de las Casas testified before the Crown that the encomienda violated its own stated justification, constituting corroboration from outside the beneficiary set that the founding religious rationale was pretextual rather than operative.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.93, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very high (0.93) and rising sharply in the early period (0.72 to 0.93 over the first third of the interval) because the encomienda system intensified as colonial administration matured from initial conquest into settled extraction infrastructure — this mirrors the historical pattern where early chaotic conquest gave way to systematized forced-labor institutions. Suppression is likewise very high (0.90) and structural: military conquest, the Requerimiento's legal fiction of 'offered' submission, and the total absence of any indigenous forum for contesting the title chain. Theater ratio starts moderately high (0.55) reflecting the genuine theatrical performance of the Requerimiento reading ceremony (read in a language the audience could not understand, satisfying a legal formality) and settles lower as administration matures into direct coercion requiring less ritual justification, ticking back up later (0.42) as reform pressure (New Laws, Valladolid debate) required renewed performative justification of the system's legitimacy. Accessibility collapse is authored lower than a pure mountain (0.35) because alternatives to submission — flight, armed resistance, strategic alliance-switching among rival conquistador factions — persisted throughout the period even though they were costly and frequently fatal; this is not a natural-law-style total collapse. Resistance is authored high (0.80) reflecting sustained indigenous military resistance, flight, and the documented internal Church dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish Crown and colonial administration sit at the full-beneficiary end: they authored the claim, collect the extraction, and control the enforcement apparatus with maximal exit options (arbitrage — able to reshape the terms of extraction at will). Encomenderos are direct beneficiaries with mobile exit (they can abandon a grant, relocate, or return to Spain). Indigenous populations and encomienda laborers sit at the full-target end: trapped exit, powerless power atom, and no standing within the legal framework that dispossesses them — directionality here should derive to very high d, and no override is needed because the beneficiary/victim declarations already capture the asymmetry cleanly. Missionary orders are split: some benefited from mission labor systems paralleling encomienda extraction (beneficiary role), while dissenting clergy occupy an excluded-from-power but morally invested position — hence the dual role authored for that stakeholder group is intentionally NOT collapsed into a single seat, since the historical record shows genuine internal division.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding inter-crown war; providing theological justification for subjugation contingent on genuine evangelization) is authored as DEAD specifically on the indigenous-subjugation half: the evangelization justification was contested from within the Church's own ranks as pretextual almost immediately (Montesinos's 1511 sermon, Las Casas's decades of advocacy), while the extraction machinery not only persisted but intensified. This is exactly the mismatch the R5 genealogy interview is designed to surface — founding_problem_status=dead paired with disappearance_verdict=world_rearranges signals a zombie/capture pattern: the arrangement outlived the problem it claimed to solve and was sustained by the interests it had created (encomenderos, colonial administration) rather than by the original justification. Classifying this as snare (rather than tangled_rope) is deliberate: while the inter-crown coordination function was genuine for the two crowns and the Church, the reading being authored HERE is specifically the Spanish-side subjugation function, where no coordination benefit accrues to the payer population at all — the coordination that exists (crown-to-crown, crown-to-Church) is a different constraint occupying a different seat, not something indigenous populations were coordinated INTO.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_scope_ambiguity,
    'Did the Pope, under the theological and legal doctrines of the period, possess genuine temporal authority to grant title over inhabited lands and their peoples to a Christian monarch, or was this an assertion of authority that exceeded any coherent doctrinal basis even by the standards of the time?',
    'Comparative analysis of contemporaneous canon law scholarship (e.g., the School of Salamanca''s later critique via Francisco de Vitoria, who argued indigenous peoples held natural dominion and that the papal grant could not extinguish it) against the Crown''s operative legal theory at the time of conquest.',
    'If the grant is read as doctrinally incoherent even by contemporary standards, the ''legitimation'' function is pure legal theater masking pre-existing conquest intent, sharpening the snare classification. If the grant reflected then-coherent (if now-rejected) doctrine, the coordination function among Christian powers was more structurally genuine, though this does not change the extraction experienced by indigenous victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_scope_ambiguity, conceptual, 'Whether the papal grant''s authority claim was doctrinally coherent or a post-hoc legal fiction for conquest already underway.').

omega_variable(
    coordination_function_locus_ambiguity,
    'Is the inter-crown war-avoidance coordination function structurally separable from the indigenous-subjugation extraction function, or does the demarcation line''s coordination value depend entirely on the extraction it enables (i.e., the crowns only needed to coordinate because both intended extraction)?',
    'Examine whether either crown pursued or would have accepted a demarcation line that excluded extraction/conquest rights (pure exploration/trade zones without subjugation authority) — historical negotiating record and subsequent crown correspondence would indicate whether extraction was assumed as the point of the exercise from the outset.',
    'If the coordination function is inseparable from the extraction intent, the entire kernel (both readings) should be read as jointly extractive from its origin, weakening any claim that either reading has a genuinely separable rope-like coordination core. If separable, the Portuguese reading''s coordination-dominant character is more defensible as structurally distinct from this reading''s extraction-dominant character.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_locus_ambiguity, conceptual, 'Whether inter-crown coordination and indigenous extraction were structurally fused from the kernel''s origin or genuinely separable functions.').

omega_variable(
    encomienda_reform_trajectory_uncertainty,
    'Did the New Laws of 1542 and subsequent reform efforts represent a genuine narrowing of the extraction (partial resolution) or primarily a relabeling that left the underlying extraction rate materially unchanged (theater absorbing reform pressure)?',
    'Quantitative analysis of indigenous labor-tribute burden and mortality rates before and after the New Laws'' partial implementation and subsequent partial repeal, region by region.',
    'If reform was substantive, the mid-to-late interval extractiveness decline (0.95 to 0.88) authored in the measurements is a real structural shift; if reform was primarily nominal, the true extractiveness trajectory may not have declined at all and the theater_ratio uptick at t=70 better explains the entire apparent decline as relabeling rather than substantive change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encomienda_reform_trajectory_uncertainty, empirical, 'Whether documented colonial reform efforts substantively reduced extraction or primarily reclassified it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(tord_tr_t15, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 15, 0.45).
narrative_ontology:measurement(tord_tr_t30, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 30, 0.38).
narrative_ontology:measurement(tord_tr_t50, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 50, 0.35).
narrative_ontology:measurement(tord_tr_t70, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 70, 0.42).
narrative_ontology:measurement(tord_tr_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(tord_be_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(tord_be_t15, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 15, 0.85).
narrative_ontology:measurement(tord_be_t30, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 30, 0.93).
narrative_ontology:measurement(tord_be_t50, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 50, 0.95).
narrative_ontology:measurement(tord_be_t70, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 70, 0.9).
narrative_ontology:measurement(tord_be_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(tord_su_t15, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(tord_su_t30, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(tord_su_t50, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 50, 0.92).
narrative_ontology:measurement(tord_su_t70, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 70, 0.88).
narrative_ontology:measurement(tord_su_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.05).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).

% DUAL FORMULATION NOTE:
% This constraint and portuguese_exploration_legitimation are sibling readings of the same tordesillas_demarcation_kernel — the same 1494 treaty text and papal bulls read for two structurally distinct functions. This file (spanish_conquest_legitimation) authors the high-extraction, victim-bearing reading: conquest license and subjugation west of the line, with indigenous populations as the direct extraction target and the encomienda system as the extraction mechanism. The sibling authors the rival-exclusion/exploration-confirmation reading east of the line, which has a much lower ε and a primarily inter-sovereign coordination function with no comparable indigenous-victim structure in its own account (the Portuguese colonial project in Brazil has its own, separately-authored extraction dynamics, not carried by that sibling story). The two are linked via affects_constraints because the same demarcation line's enforcement and legitimacy in one theater structurally affects the credibility and precedent value of the claim in the other — a challenge to the papal grant's authority in the Spanish theater (e.g., the Vitoria critique) has downstream pressure on the Portuguese reading's legitimacy claim as well.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
