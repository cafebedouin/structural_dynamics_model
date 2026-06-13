% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Laws as Adaptive Constitutional Fiction
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Sparta's Lycurgan laws are publicly framed as sacred, unchangeable, and
 *   ordained by the gods—a mountain claim of constitutional immutability.
 *   This reading rejects that framing and argues instead that the laws
 *   functioned as a rope-level coordination mechanism whose success depended
 *   entirely on quiet institutional adaptation. The ephorate, kingship, and
 *   oracular priesthood maintained a fiction of immutability while
 *   interpreting the laws to permit land reallotment, economic flexibility,
 *   and military innovation. The constraint's extractive character
 *   (suppression of helots and perioeci, concentration of resources in the
 *   warrior class) was sustained not by the laws' actual rigidity but by the
 *   belief in that rigidity—a noble lie that allowed elite governance to
 *   persist while appearing unchangeable. The reading claims that demographic
 *   decline, military defeats, and eventual Spartan collapse resulted not
 *   from over-rigid adherence to the laws, but from the failure of quiet
 *   adaptation to keep pace with changing material conditions—the fiction
 *   eventually became so divorced from practice that both the myth and the
 *   system fractured together.
 *
 * KEY AGENTS:
 *   - ephorate_council: institutional agenda-setter with interpretive arbitrage; adapts through reinterpretation while maintaining immutability fiction
 *   - kingship_institution: dual executive with flexibility in military practice; constrained by dual structure but empowered by interpretive discretion
 *   - spartan_warrior_class: beneficiary whose identity is fused to Lycurgan austerity; they benefit from the equal landholding and status the laws enforce
 *   - helot_population: powerless victims trapped in perpetual servitude under laws declared sacred and unchangeable
 *   - perioeci_class: constrained middle class whose economic expansion is quietly permitted while nominally forbidden
 *   - oracular_priesthood: beneficiary and agenda-setter who derive authority from sanctifying the laws as divine
 *   - democratic_reformers: excluded voices whose formal amendment proposals are forbidden by the constraint's rhetoric of immutability
 *   - ancient_historians: analytical observers who document both the public myth and the private adaptation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.58).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.72).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Laws as Adaptive Constitutional Fiction").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political/constitutional").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '3ccb221b-9691-45e6-b9e3-a0dce1f4e71b').
narrative_ontology:cs_kernel_codification('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', formalized).
narrative_ontology:cs_authority_grounding('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', extraction).
narrative_ontology:cs_interpretation_layer_present('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b').
narrative_ontology:cs_reading_relation('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', foundational, immutability_fiction_enables_adaptation).
narrative_ontology:cs_axiom_status(immutability_fiction_enables_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', immutability_fiction_enables_adaptation, instrumental).
narrative_ontology:cs_axiom('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', secondary, elite_interpretive_discretion_reconciles_law_and_practice).
narrative_ontology:cs_axiom_status(elite_interpretive_discretion_reconciles_law_and_practice, holdable).
narrative_ontology:cs_axiom_grounding('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', elite_interpretive_discretion_reconciles_law_and_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', formal_legal_immutability_with_interpretive_flexibility).
narrative_ontology:cs_drift_state('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', late_spartan_period_empire_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ccb221b-9691-45e6-b9e3-a0dce1f4e71b', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_warrior_class).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephorate_council).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, kingship_institution).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, perioeci_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, oracular_priesthood).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, constitutional_immutability_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, noble_lie_legitimacy).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, interpretive_flexibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The warrior class benefits from laws that enforce collective discipline, equal landholding, and mandatory military service. Their identity is constituted by Lycurgan austerity and martial prowess. They receive education, land tenure, and social status under the system. Exit would mean abandoning the identity that defines them as Spartans.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_warrior_class, beneficiary,
    powerful, generational, identity_locked, local).

% The ephors administer the laws and conduct periodic reviews of Lycurgan fidelity. They possess interpretive authority over statutory language without formal amendment power. They quietly interpret rules to permit adaptation—reallotting land, adjusting military age thresholds, tolerating merchant activity—while maintaining the public fiction of immutable law. This flexibility allows institutional persistence while avoiding formal breach of the constitution.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephorate_council, agenda_setter,
    institutional, generational, arbitrage, local).

% The two kings benefit from laws that enshrine monarchy and military authority. They also exercise interpretive discretion in military matters, commanding flexible logistics and personnel decisions while the laws appear unchanged. They are constrained by the dual kingship structure and by popular belief in Lycurgan immutability; they cannot formally amend, but they can quietly practice adaptive governance.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, kingship_institution, agenda_setter,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, kingship_institution, beneficiary).

% Helots are bound to the land, subject to periodic purges, and required to supply agricultural surplus and military service. The laws that define their status as perpetual serfs are presented as sacred and unchangeable. They bear the extraction cost without voice in interpretation. Exit is death or slavery in foreign lands.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, payer,
    powerless, biographical, trapped, local).

% The perioeci (dwellers-around) are free non-citizens, crafts-people and merchants in towns adjacent to Sparta. The laws nominally restrict their economic activity to enforce the warrior class's austerity. The ephorate quietly tolerates merchant expansion and craft specialization while maintaining the fiction of Lycurgan purity, allowing the perioeci limited growth while keeping them subordinate.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, perioeci_class, payer,
    moderate, biographical, constrained, local).

% The Delphic Oracle and Spartan priests derive authority and wealth from certifying Lycurgan laws as divinely ordained and unalterable. They pronounce that any formal amendment would incur divine wrath. This sanctification creates the constraint that permits quiet adaptation: the laws are sacred (cannot be amended), so interpretation must suffice, so adaptation flows through a hidden channel that maintains the illusion.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, oracular_priesthood, beneficiary,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, oracular_priesthood, agenda_setter).

% Internal reformers who advocate formal amendment or abolition (present in Sparta's later periods) are structurally barred from the conversation: the laws are declared sacred, proposals for formal change are treated as sacrilege, and reformers are exiled or executed. Their exclusion is enforced by the constraint's own rhetoric of immutability.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, democratic_reformers, excluded,
    moderate, biographical, trapped, local).

% Later historians (Plutarch, Xenophon) observe the constraint from outside, documenting both the public claim of immutability and the private evidence of adaptation. They see the gap and record it, though their analysis is filtered through the constraint's own rhetoric.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ancient_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, ephorate_council).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enforces collective military discipline, equal landholding among warriors, and subordination of economic activity to martial readiness. Solves the coordination problem of maintaining a permanent military state through shared sacrifice and ideological commitment to the laws themselves as sacred objects.
% TRANSFER_FUNCTION: Moves agricultural surplus from helots and perioeci to the warrior class and institutional elite; moves military service and labor compliance from all subordinate classes; moves interpretive authority and institutional flexibility to the ephorate and kingship while the public believes the laws forbid any such authority.
% ABSENT_VOICES: Helots and perioeci have no formal voice in interpretation or amendment; democratic reformers are structurally excluded and their proposals are framed as sacrilege. These groups would testify that the laws operate as rigid extraction enforced against them, and that quiet adaptation benefits only the elite—but the constraint's rhetoric of immutability prevents them from even being heard proposing change.
% DISAPPEARANCE_RATIONALE: If the fiction of immutability dissolved and formal amendment became possible, the ephorate would lose the hidden channel through which it adapts governance. The warrior class would lose the ideological claim that binds them to austerity. Helots would gain standing to demand formal amendment of their status. The entire institutional structure would reorganize around explicit choice rather than sacred immutability—a radical political rupture.
% FOUNDING_PROBLEM: Sparta's founders (or reformers, if Lycurgus was legendary) faced a security crisis: a small warrior elite controlling a subjugated helot population risked constant internal conflict and elite disobedience. The laws aimed to solve this by replacing individual choice with collective discipline and by making the legal structure itself sacred, beyond challenge.
% FOUNDING_PROBLEM_CORROBORATION: Xenophon and Plutarch document the original security rationale and the subsequent decay of strict compliance. Modern historians (Cartledge, Figueira) debate whether the founding problem was real or invented after the fact to justify oppression. No external authority outside the Spartan elite corroborates the problem as framed by the constraint itself—the problem is known only through the constraint's own rhetoric.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading's extractiveness (0.58, rising to 0.63 at peak) is moderate-to-high because the system concentrates wealth and military power while denying helots and perioeci any formal right to amendment. The theater ratio (0.68 at terminal, rising from 0.42) is the core diagnostic: as time passes, the divergence between the public claim of immutability and the private reality of adaptation grows—more of what the constraint does is theatrical maintenance of the myth rather than functional coordination. The measurement series shows theater rising faster than extractiveness in later periods (t=320 onward), indicating that the constraint began as genuine coordination (Lycurgus era, t=0) and devolved into predominantly performative enforcement of a fiction. Suppression requirement rises through the interval (0.58 to 0.72), reflecting that as material conditions changed (trade, wealth accumulation, demographic pressure), maintaining the fiction of immutability required increasing coercive effort. The final dip (t=400, suppression to 0.72) marks the period when the fiction itself began to crack and active suppression of reformers was abandoned—the constraint was losing coherence.
 *
 * PERSPECTIVAL GAP:
 *   The ephorate and kingship compute the constraint as rope: they coordinate military discipline and land distribution while maintaining flexibility through interpretation. The warrior class computes it as mountain: they genuinely believe the laws are sacred and unchangeable, which motivates their adherence to austerity. The helots and perioeci compute it as snare: they see only the extraction and suppression, with no exit and no voice in the rules that govern them. The oracle computes it as mutually beneficial (they certify immutability, they are paid; the elites get the immutability fiction, they are enriched)—a rope at their seat. The engine will compute different classifications per seat from the structural data: beneficiary seats (ephorate, kings, oracle) derive low directionality; victim seats (helots, perioeci) derive high directionality, producing high effective extraction at those seats. The perspectival gap is the reading's central claim: the constraint appeared to be a mountain (natural law, immutable) from the outside, but operated as a rope (adaptive coordination) at the administrative seat, enabling extraction at the subordinate seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim declarations and exit options. The ephorate and kingship declare as agenda-setters with arbitrage-grade exit (they can reinterpret the laws to suit circumstances)—d near 0.0 to 0.2, full beneficiary end. The warrior class declares as beneficiary with identity_locked exit (they cannot leave without ceasing to be Spartan)—d near 0.3 to 0.4, partial subsidy. The oracular priesthood declares as beneficiary-agenda-setter with arbitrage (they can interpret divine will, they are wealthy)—d near 0.1 to 0.3. The helots and perioeci declare as victims with trapped exit—d near 0.8 to 1.0, full target end. The democratic reformers (excluded) would compute as high-d targets if they were in the conversation, but their exclusion is itself the constraint's mechanism, so they do not register in the beneficiary/victim derivation. The directionality profile shows a classic extraction pattern: powerful institutional seats at low d (beneficiaries), powerless seats at high d (victims), with the constraint's persistence depending on the victims' lack of alternative and the beneficiaries' stake in maintaining the immutability fiction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT resolve mandatrophy in the classical sense. The founding problem (elite security against helot uprising and warrior defection) is structurally live as long as the hierarchy persists—the constraint's mandated function (enforcing collective discipline) never becomes obsolete. The mandatrophy question instead takes the form: does the elite BELIEVE the founding problem requires immutable law, or do they know the laws are being reinterpreted? This reading asserts the latter—the elite know (or learn, over generations) that adaptation is compatible with stability, and thus the constraint's mandate (enforcing obedience through sacred immutability) decouples from its actual operation (maintaining stability through flexible interpretation). Mandatrophy is avoided only by the fiction: as long as the illusion of immutability is maintained, the masses obey (they think the laws cannot change), and the elite can quietly adapt (they know the laws can be reinterpreted without formal amendment). Collapse occurs when the fiction breaks—when democratic reformers gain enough voice to make formal amendment thinkable, or when material conditions change so fast that interpretation cannot keep pace—then mandatrophy becomes real: the laws are seen as rigid and obsolete, or as false legitimation, and the system fractures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ephoral_intentionality,
    'Did the ephorate and kingship consciously deploy the immutability fiction as a tool for flexible governance, or did they gradually discover that interpretation could serve adaptation?',
    'Historical text analysis of ephoral decrees and interpretive pronouncements; comparative study of formal constitutional change versus actual practice drift across the interval; archaeological evidence of economic and military variation inconsistent with literal law enforcement.',
    'If conscious deployment, the constraint is a deliberately engineered rope sustained by strategic illusion. If gradual discovery, it is a rope that emerged from the interplay of rigidity and survival pressure. Either way, the adaptive function is confirmed; the difference is in elite agency and deliberateness. If neither (rigid enforcement was attempted), the reading collapses toward sacral_fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ephoral_intentionality, empirical, 'Whether institutional flexibility was deliberate strategy or emergent adaptation.').

omega_variable(
    warrior_class_belief,
    'Did the warrior class actually believe the laws were immutable, or did they know (or suspect) that elite reinterpretation was happening?',
    'Textual evidence from Spartan internal discourse (rare, mediated through later historians); comparative study of warrior behavior consistent with belief in immutability versus behavior indicating cynicism or awareness of adaptation; patterns of compliance and resistance in the historical record.',
    'If genuine belief, the illusion worked perfectly and coordination was sustained by widespread false consciousness. If widespread cynicism, the constraint operated differently—warriors consented to subordination knowing the laws were flexible, not sacred, which implies a different power dynamic. If mixed (some believed, some knew), the constraint operated as a collective-action trap: individuals knew the system was flexible but coordinated on the public claim of immutability because defection would expose the lie.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrior_class_belief, empirical, 'Whether the warrior class internalized the immutability claim or recognized it as fiction.').

omega_variable(
    oracular_complicity,
    'How autonomous was the Delphic Oracle in sanctifying Spartan law, and how much was it a tool of Spartan elite manipulation?',
    'Study of oracle pronouncements on Spartan law relative to other poleis; evidence of Spartan gifts, influence, or threat at Delphi; comparative analysis of how other city-states used the oracle to legitimize legal claims.',
    'If the oracle was autonomous or genuinely believed in Lycurgan divinity, the immutability claim had independent sacred authority. If the oracle was manipulated or coopted, the immutability claim was engineered by the elite itself—increasing the reading''s claim that the whole system was a fiction maintained for governance purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracular_complicity, empirical, 'Whether the oracle independently sanctified the laws or was a tool of Spartan elite strategy.').

omega_variable(
    demographic_decline_causation,
    'Did Spartan demographic decline result from the failure of quiet adaptation to keep pace with material change (this reading), or from rigid over-enforcement of unchangeable laws (demographic_trap reading)?',
    'Historical analysis of population records, land-holding patterns, military capacity, and economic indicators across the interval; comparison of periods when adaptation was possible and successful with periods when rigidity was enforced; study of Spartan responses to resource pressure and evidence of whether they attempted to adapt.',
    'If decline resulted from adaptation-failure (the adaptation was real but insufficient), the reading is supported and demographic collapse is a secondary effect of the fiction breaking down. If decline resulted from enforced rigidity despite pressure to adapt, the demographic_trap reading is supported and this reading''s claim of successful adaptation is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_decline_causation, empirical, 'Whether Spartan collapse resulted from failed adaptation or rigid non-adaptation.').

omega_variable(
    reading_kernel_distinction,
    'Is the core distinction between this reading (adaptive_fiction) and the sacral_fidelity reading a difference in what actually happened (the laws were or were not adapted), or a difference in framing and interpretation of the same facts?',
    'Careful textual and archaeological analysis of whether the laws actually changed in practice; establishment of a baseline of what literal enforcement would have looked like and comparison to observed practice; determination of whether divergence is adaptation or is instead alternative interpretation of the same laws.',
    'If the reading disagreement is empirical (about facts), then evidence of adaptation falsifies sacral_fidelity. If the disagreement is interpretive (about how to read the same facts), then both readings remain live and the kernel contest is conceptual, not resolvable by further data. If facts remain ambiguous, the kernel remains contested and both readings remain viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether the competing readings of Lycurgan immutability are factually distinguishable or conceptually under-determined by available evidence.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72 to 0.72 across the interval) structural (external barriers: legal restrictions, physical force, resource denial) or internalized (the helot and perioeci belief in the laws'' justice and unchangeability)?',
    'Historical evidence of escape attempts, rebellion, and their frequency over time; study of helot and perioeci petitions for reform or redress; comparison of compliance patterns consistent with structural suppression versus patterns consistent with internalized acceptance; analysis of post-liberation helot behavior to test whether suppression persisted after the constraint was removed.',
    'If suppression is largely internalized, the constraint''s real extractiveness is higher than the measured suppression suggests—the victims carry the suppression with them and the fiction of immutability is deeply internalized. If suppression is structural, the victims are aware they are trapped and do not consent; the fiction is an elite tool, not a shared belief. The difference affects whether the constraint operated as rope-like (consensus-based coordination) or snare-like (coerced extraction) at the subordinate seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of helots and perioeci was structural external force or internalized belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(lycu_tr_t0, observed).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement_basis(lycu_tr_t80, observed).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 160, 0.54).
narrative_ontology:measurement_basis(lycu_tr_t160, observed).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 240, 0.61).
narrative_ontology:measurement_basis(lycu_tr_t240, observed).
narrative_ontology:measurement(lycu_tr_t320, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 320, 0.68).
narrative_ontology:measurement_basis(lycu_tr_t320, observed).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 400, 0.68).
narrative_ontology:measurement_basis(lycu_tr_t400, observed).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(lycu_be_t0, observed).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement_basis(lycu_be_t80, observed).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 160, 0.56).
narrative_ontology:measurement_basis(lycu_be_t160, observed).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 240, 0.61).
narrative_ontology:measurement_basis(lycu_be_t240, observed).
narrative_ontology:measurement(lycu_be_t320, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 320, 0.63).
narrative_ontology:measurement_basis(lycu_be_t320, observed).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement_basis(lycu_be_t400, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(lycu_su_t0, observed).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(lycu_su_t80, observed).
narrative_ontology:measurement(lycu_su_t160, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 160, 0.67).
narrative_ontology:measurement_basis(lycu_su_t160, observed).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 240, 0.71).
narrative_ontology:measurement_basis(lycu_su_t240, observed).
narrative_ontology:measurement(lycu_su_t320, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 320, 0.75).
narrative_ontology:measurement_basis(lycu_su_t320, observed).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 400, 0.72).
narrative_ontology:measurement_basis(lycu_su_t400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.12).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% The kernel 'lycurgan_laws' contains three structurally distinct constraint readings: adaptive_fiction_reading (this constraint—Lycurgan immutability as a fiction masking quiet institutional adaptation, ε≈0.58), sacral_fidelity_reading (the laws were truly sacred and rigidly enforced, ε≈lower, a mountain claim), and demographic_trap_reading (rigidity caused demographic collapse, ε≈higher extraction from failed enforcement). The three readings differ in their core claim about whether the laws were actually immutable and whether adaptation occurred. This reading claims adaptation WAS the reality (disguised by the immutability fiction), distinguishing it from sacral_fidelity (which asserts genuine immutability) and from demographic_trap (which asserts rigidity caused collapse). Network edges link all three; they should be compared together to test which ε value is most consistent with the historical record.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
