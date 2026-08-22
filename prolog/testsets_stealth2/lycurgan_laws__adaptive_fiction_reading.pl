% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Noble Lie Masking Covert Adaptation (Adaptive-Fiction Reading)
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The Lycurgan settlement was publicly constituted as finished and
 *   inviolable: an orally fixed rhetra, an oath said to bind the citizens
 *   until the founder's return from Delphi (from which he never returned), a
 *   ban on writing the laws down, and religious sanction attaching to any
 *   proposal of change. This reading holds that the immutability claim
 *   functioned as a politically engineered noble lie: beneath it, the five
 *   annual overseers, the council of elders, and the kings continuously
 *   adjusted the order through interpretation, precedent, wartime suspension,
 *   and property practice — while the public letter stayed frozen. The
 *   arrangement thereby delivered centuries of constitutional stability AND
 *   concentrated adaptive capacity at the top: established families bent
 *   inheritance and landholding practice under an equality rhetoric that
 *   stayed ritually intact, ordinary citizens bore frozen obligations with no
 *   legitimate channel to request relief, and the bound population stood
 *   wholly outside the bargain. On this reading the late demographic collapse
 *   reflects enforcement failure — the machinery that once silently adapted
 *   could no longer deliver even that — rather than the rigidity the sibling
 *   reading blames. KEY AGENTS (by structural relationship): -
 *   ephorate_officeholders: Agenda-setter and collecting beneficiary
 *   (institutional/constrained) — administers the settlement; their
 *   interpretations are the covert adaptation channel - hereditary_kings:
 *   Dual-positioned beneficiary-payer (powerful/identity_locked) — sanctified
 *   by the fiction, supervised by the magistrates - gerousia_elders:
 *   Guardian-beneficiaries (powerful/identity_locked) — lifetime seats
 *   dependent on the settlement's sanctity - elite_landholding_families:
 *   Principal rent-collecting beneficiaries (powerful/arbitrage) — estate
 *   flexibility under equality rhetoric - rank_file_spartiates: Primary
 *   citizen payers (moderate/identity_locked) — frozen obligations, no voice
 *   in adjustment - helot_populace: Primary extramural payers
 *   (powerless/trapped) — bound labor outside the bargain entirely -
 *   would_be_reformers: Excluded insiders (powerful/trapped) — able to speak
 *   only in the borrowed accent of restoration - classical_commentators:
 *   Analytical observers (analytical/analytical) — document the gap between
 *   self-description and operation Kernel-family note: the colloquial label
 *   'the immutable laws of Lycurgus' decomposes into three structurally
 *   distinct constraints sharing one referent — the operated Lycurgan order.
 *   The sacral_fidelity_reading authors low epsilon (sincerely held divine
 *   ordinance); the demographic_trap_reading authors high epsilon (fatal
 *   brittleness); THIS reading authors moderate epsilon (0.63) for the
 *   fiction-plus-covert-adaptation arrangement as this reading assesses it.
 *   Same referent, reading-indexed values; the files are linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ephorate_officeholders: agenda-setter and collecting beneficiary (institutional/constrained) — administers the settlement and collects interpretive discretion
 *   - hereditary_kings: dual-positioned beneficiary-payer (powerful/identity_locked)
 *   - gerousia_elders: guardian-beneficiary (powerful/identity_locked)
 *   - elite_landholding_families: principal rent-collecting beneficiary (powerful/arbitrage)
 *   - rank_file_spartiates: primary citizen payer (moderate/identity_locked)
 *   - helot_populace: primary extramural payer (powerless/trapped)
 *   - would_be_reformers: excluded insider (powerful/trapped)
 *   - classical_commentators: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.63).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.46).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Noble Lie Masking Covert Adaptation (Adaptive-Fiction Reading)").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220').
narrative_ontology:cs_kernel_codification('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', formalized).
narrative_ontology:cs_authority_grounding('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', lineage).
narrative_ontology:cs_interpretation_layer_present('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220').
narrative_ontology:cs_reading_relation('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', lycurgan_laws__sacral_fidelity_reading, influences).
narrative_ontology:cs_reading_relation('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_axiom('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', foundational, immutability_claim_is_operative_fiction).
narrative_ontology:cs_axiom_status(immutability_claim_is_operative_fiction, holdable).
narrative_ontology:cs_axiom_grounding('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', immutability_claim_is_operative_fiction, empirically_contingent).
narrative_ontology:cs_axiom('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', secondary, authorized_interpretation_absorbs_revision).
narrative_ontology:cs_axiom_status(authorized_interpretation_absorbs_revision, holdable).
narrative_ontology:cs_axiom_grounding('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', authorized_interpretation_absorbs_revision, conventional).
narrative_ontology:cs_reference_frame('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', rhetra_immutable_letter_adaptive_practice).
narrative_ontology:cs_drift_state('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', cleomenic_restoration_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('8bbcfa7a-6a61-40d3-bd2f-fdcb61cf4220', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephorate_officeholders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, gerousia_elders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, hereditary_kings).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, elite_landholding_families).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, rank_file_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, rank_file_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, hereditary_kings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five citizens elected annually to oversee the whole order: they preside over assemblies, summon and discipline kings, interpret signs and precedents, suspend penalties, convene courts, and each year proclaim a periodic state of hostility against the bound population. Their interpretations are the main channel through which settled rules get quietly adjusted to circumstance while the public letter stays frozen. At term's end they answer for their conduct before their successors. Leaving the office means returning to private life under the rules they administered.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephorate_officeholders, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, ephorate_officeholders, beneficiary).

% Two royal houses supply the army's commanders and the polity's chief priests, with authority resting on descent from the founder's settlement. Each month they swear to uphold the magistrates' terms; on campaign they can be suspended and fined. They gain standing from the claim that the founder's arrangement is inviolable, since their line is woven into it, yet they chafe under the magistrates' oversight. There is no exit from the house: kingship is birthright and identity.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, hereditary_kings, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, hereditary_kings, payer).

% Twenty-eight men over sixty, elected for life, who prepare assembly business, judge in capital cases, and can set aside assembly decisions they judge perverse. Their lifetime seats and prestige depend on the settlement's sanctity, making them its most invested guardians. Membership lasts until death; there is no stepping down and no life outside the council's standing.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, gerousia_elders, beneficiary,
    powerful, generational, identity_locked, national).

% Established families whose estates grow through inheritance practice — daughters' shares, heiress marriages, gifts, bequests — while public rhetoric keeps describing holdings as equal allotments from the founder. Property arrangements bend with circumstance for those with standing and legal skill; by the late period a large share of the land is held by a small set of such households, much of it in women's hands. Their position inside the order is secure, and the flexibility of their holdings is precisely what the frozen public letter conceals.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, elite_landholding_families, beneficiary,
    powerful, generational, arbitrage, national).

% Full citizens who completed the upbringing, contribute mess dues from their allotments, serve in the army from adulthood to old age, and send their sons through the training. They share the order's stability, standing, and military renown; but when harvests fail or estates shrink below what mess dues require, they lose standing with no legitimate way to ask that terms be adjusted — proposing change is impiety, and adjustment happens above their heads or not at all. Falling behind means dropping off the citizen rolls entirely, a social death.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, rank_file_spartiates, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, rank_file_spartiates, beneficiary).

% A bound agricultural population, families held generation to generation on Laconian and Messenian land, delivering produce to allotment-holders. The magistrates annually renew a formal state of hostility against them, and detachments of young citizens are sent among them. They have no standing in the citizen order at all. Flight or revolt are the only exits, and both are lethal — though the revolts, when they came, shook the order to its foundations.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_populace, payer,
    powerless, generational, trapped, national).

% Insiders — often royal — who conclude the settlement needs renewal and discover that open proposal is impossible: the only sellable form for change is 'restoration' of the founder's supposed original intent. Those who tried it in the late period were prosecuted, exiled, or killed. They cannot leave the order without abandoning the standing that makes reform conceivable, so they work inside and die inside.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, would_be_reformers, excluded,
    powerful, biographical, trapped, national).

% Outside analysts — visiting philosophers, exiles, later historians — who compare the order's self-description with its operation, note the gap between the equality rhetoric and the landholding reality, admire the stability, and pass judgments the participants cannot safely voice. Nothing in the order binds them; their seat is observational.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, classical_commentators, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, elite_landholding_families).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settlement solves a recurring collective-action problem: every generation contains factions that would reopen the constitutional bargain — redistribute land, curb the kings, lighten obligations. Declaring the founder's arrangement finished and inviolable, backed by an oath and religious sanction, removes constitutional bargaining from the agenda indefinitely; authorized interpreters absorb necessary adjustments quietly so the public settlement never has to be renegotiated.
% TRANSFER_FUNCTION: Moves interpretive authority and material security upward: discretion over rules flows to the magistrates and the council; estate flexibility flows to established families through inheritance practice; obedience, mess contributions, military service, and the whole agricultural surplus of the bound population flow up from ordinary citizens and the bound countryside.
% ABSENT_VOICES: Ordinary citizens with adjustment proposals and the bound population have no voice: proposal equals impiety, and the bound have no standing whatsoever. Late-period reformers could speak only in the borrowed accent of restoration. Their absence is what lets the settlement present itself as unanimous consent.
% DISAPPEARANCE_RATIONALE: If the fiction and its machinery vanished overnight, the settlement becomes openly negotiable: land claims, royal prerogatives, mess obligations, and the status of the bound population all become live questions at once, and the factions the settlement was built to freeze resume bargaining. The stability that defined the polity dissolves within a generation.
% FOUNDING_PROBLEM: After the migration and the wars of conquest, the polity faced chronic internal strife — kings against nobles against commoners — while surrounded by hostile neighbors. The founder's settlement, the oath extracted from the citizens, and the refusal to write the laws down were built to end constitutional bargaining permanently and lock a working distribution of roles in place.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: Aristotle's Politics documents the gap between the settlement's equality rhetoric and its actual landholding and treats the arrangement as long past answering its original problem; the late-period reformers' own program concedes the founding settlement no longer functions, arguing only about what should replace it; and visiting observers across the classical period attest that the arrangement persisted as inherited form rather than answered need. No corroborating source inside or outside the beneficiary set claims the founding strife still required the arrangement as it stood.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon (0.63) is authored over the standing arrangement under contest — the operated order of frozen public letter plus authorized covert adjustment — assessed by this reading's lights: a genuinely functional coordination core wrapped around concentrated adaptive rents and a wholly unfree population. Suppression (0.46, end-state) is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation. The suppression_requirement series is authored because enforcement-capacity change IS this story's traced dynamic: the machinery of sanction built up through the classical height (0.72 to 0.75) as anti-revision norms institutionalized, then collapsed (0.68 to 0.46) as material incentives, outside contact, and shrinking resources eroded compliance — the enforcement failure this reading holds responsible for the demographic decline. Theater_ratio rises monotonically (0.15 to 0.55) as the performed immutability outlived the adaptive function it covered: by the late period the forms were kept up over a hollowed machinery, crossing the 0.5 substitution threshold in the final stretch. Base extractiveness rises with the growth of bound labor and estate concentration, peaks when enforcement was still strong enough to take (0.65), and dips slightly at the end (0.63) as the machinery's capacity to extract failed along with everything else. Accessibility_collapse (0.66) is high but not total: for ordinary citizens alternatives were nearly closed (proposal equaled impiety), yet the covert channel always existed for insiders, and the fiction was in principle exposible. Resistance (0.42) is real but notably channeled THROUGH the fiction — every major reform attempt had to present itself as restoration of the founder's supposed original intent, which is itself evidence of the claim's grip. All three series run on one shared six-point grid; every tracked metric is authored at every examined time point. Coalition note: the one repeatedly realized coalition among the powerless was the bound population itself, whose regional revolts (culminating in the great mid-fifth-century uprising after the earthquake) were the order's recurrent internal emergency and a standing limit on how far extraction could push.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the magistrates' seat the arrangement is a functioning coordination machine they personally operate: adjustment happens, crises are absorbed, the polity holds — a coordination-first experience. From the bound population's seat the same structure is unmitigated taking: no standing, no exit, annual formalized hostility. From the rank-and-file citizen seat it is a gilded enclosure — real standing and stability, purchased with obligations that can never be renegotiated and a status that evaporates if harvests fail. The royal seat is genuinely dual: sanctified by the fiction it cannot exit, constrained by the magistrates it serves beside. The analytical observers see the gap between self-description and operation that participants were structured not to voice. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: the magistrates (collect interpretive discretion), the council elders (lifetime authority premised on sanctity), the royal houses (dynastic legitimation), and the established landholding families (estate flexibility under equality rhetoric). Victim declarations map to high-directionality seats: rank-and-file citizens (frozen obligations, status-contingent survival) and the bound population (total exposure, zero standing). Two overrides are declared where the automatic derivation would misplace d. First, the kings: beneficiary status plus identity-locked exit would derive a near-beneficiary d, but the monthly oaths to the magistrates, fines, and campaign suspensions make them half-targets — overridden to 0.45. Second, the rank-and-file: payer status plus identity-locked exit would derive a near-full-target d, but centuries of shared stability, military preeminence, and citizen standing made them partial beneficiaries of the same structure that froze them — overridden to 0.72. The bound population needs no override: payer, powerless, trapped derives full-target d, which is descriptively exact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — chronic internal constitutional strife in a besieged polity — was genuinely solved, and solved for centuries: the settlement froze constitutional bargaining and delivered stability that outside observers admired. But the problem died long before the arrangement did. By the late period the founding problem is dead while the world still rearranges around the arrangement's disappearance — the mismatch signature of a mandate outliving its function. The trajectory this reading traces is coordination decaying into theatrical maintenance: the fiction began as load-bearing (expectation-locking with real adaptive capacity beneath), and ended as performance over a machinery that could no longer adapt or enforce (theater_ratio crossing 0.5 in the final interval, suppression_requirement collapsing). The classification discipline prevents two opposite mislabelings: reading the arrangement as pure taking misses the centuries of genuine service the fiction rendered; reading it as benign coordination misses the captured adaptive rents and the population held outside the bargain. The honest terminal description is inertial performance — the end-state profile the temporal series documents — while the claimed tangled_rope describes the arrangement across its operating life, when coordination and extraction ran through the same structure and enforcement was real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates the adaptive_fiction_reading of the lycurgan_laws kernel; what structurally changes if either sibling reading is adopted instead?',
    'Adopting the sacral_fidelity_reading re-authors epsilon over the same referent at sincere-adherence levels and moves the type toward a mountain-with-beneficiaries (false-summit candidate); adopting the demographic_trap_reading raises epsilon sharply and moves the type toward pure extraction. Cross-reading comparison over the fixed referent is the meta-analytic product.',
    'The classification of the identical standing arrangement flips across readings; per-seat classifications shift as victim sets and enforcement premises change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: one reading of a contested kernel; siblings would re-author epsilon and type.').

omega_variable(
    enforcement_failure_vs_rigidity,
    'Did the collapse of the citizen body trace to failure of the settlement''s enforcement machinery (this reading''s claim) or to genuine unrevisability of the laws (the demographic_trap_reading''s claim)?',
    'Date the documented covert adaptations (ephoric interpretation, the early crooked amendment adding the magistrates, wartime suspensions) against the citizen-roll decline; test whether adjustment channels were available and used yet insufficient, or effectively unavailable.',
    'If adaptations were real and routine yet decline proceeded, this reading strengthens and the fiction''s covert channel is judged inadequate adaptation; if no significant adaptation occurred, the sibling reading wins and the immutability claim was operative rather than fictional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_failure_vs_rigidity, empirical, 'Causal contest between this reading and the demographic-trap sibling over the decline''s mechanism.').

omega_variable(
    land_concentration_share,
    'How much of the citizen-body decline traces to estate concentration through heiress inheritance and bequest inside established families, versus war losses, the mid-fifth-century earthquake, and voluntary abstention?',
    'Prosopography of attested landholding and citizen-roll attrition across the classical and Hellenistic periods.',
    'If concentration dominates, the covert adaptation channel operated substantially as a rent-collection mechanism for established families, raising effective extraction and pushing the end-state toward captured maintenance; if exogenous shocks dominate, the channel was less predatory than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_concentration_share, empirical, 'Decomposition of the decline between captured adaptation and exogenous shock.').

omega_variable(
    fiction_sincerity_distribution,
    'Across ranks and periods, who held the immutability claim sincerely and who held it as an instrument — did the magistrates and council know the laws bent while the rank and file believed?',
    'Register the sources'' attributions of knowledge and belief: transmission of the founder''s oath story, ephoral behavior when rules inconvenienced them, and the reformers'' need to disguise innovation as recovery.',
    'If insiders predominantly knew, the fiction was designed cover for concentrated advantage; if belief was broad even among elites, the arrangement was self-deceiving coordination whose asymmetries emerged unintentionally — changing whether the extraction reads as designed or emergent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiction_sincerity_distribution, empirical, 'Distribution of sincere belief versus instrumental profession of the immutability claim.').

omega_variable(
    restoration_frame_grip,
    'Does the late-period reformers'' confinement to restoration-only framing reflect the fiction''s cognitive grip on insiders (exit barred by identity) or merely tactical cover in a hostile audience?',
    'Compare the reformers'' recoverable private positions and conduct with their public programs, and compare with contemporaneous outsider proposals unconstrained by the founder frame.',
    'If grip, the identity-locked coding of insider seats is confirmed and exit was doubly barred — suppression partly internalized; if cover, suppression is purely structural and the fiction''s hold was weaker than its performance suggested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_frame_grip, conceptual, 'Whether the restoration-only frame evidences internalized lock or strategic necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_af_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lycurgan_af_tr_t120, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement(lycurgan_af_tr_t240, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 240, 0.3).
narrative_ontology:measurement(lycurgan_af_tr_t360, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 360, 0.38).
narrative_ontology:measurement(lycurgan_af_tr_t480, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 480, 0.48).
narrative_ontology:measurement(lycurgan_af_tr_t600, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 600, 0.55).

% Extraction over time
narrative_ontology:measurement(lycurgan_af_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(lycurgan_af_be_t120, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 120, 0.51).
narrative_ontology:measurement(lycurgan_af_be_t240, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 240, 0.56).
narrative_ontology:measurement(lycurgan_af_be_t360, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 360, 0.59).
narrative_ontology:measurement(lycurgan_af_be_t480, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 480, 0.65).
narrative_ontology:measurement(lycurgan_af_be_t600, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 600, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_af_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(lycurgan_af_su_t120, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 120, 0.74).
narrative_ontology:measurement(lycurgan_af_su_t240, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 240, 0.75).
narrative_ontology:measurement(lycurgan_af_su_t360, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 360, 0.68).
narrative_ontology:measurement(lycurgan_af_su_t480, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 480, 0.55).
narrative_ontology:measurement(lycurgan_af_su_t600, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 600, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, spartan_helot_subjugation_regime).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, spartan_agoge_obligation_system).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the immutable laws of Lycurgus' decomposes per the epsilon-invariance principle into three readings of one kernel, each a separate story with its own epsilon, victim structure, and type over the shared referent of the operated Lycurgan order. The sacral_fidelity_reading is upstream (the sincere belief-stock the fiction exploited and depended on); this adaptive_fiction_reading reads the operated reality of fiction-plus-covert-adaptation; the demographic_trap_reading reads the failure mode (rigidity versus enforcement failure as the decline's cause). This file links both siblings and two downstream dependencies: the bound-labor regime, whose perpetuation the settlement's machinery underwrote (including the annually renewed formalized hostility), and the upbringing-and-mess obligation system, whose burdens the frozen public letter sanctified. Contamination propagates along these edges: erosion of the sincerity stock undermines this arrangement's cover, and this arrangement's enforcement failure feeds the sibling collapse narrative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, powerful, 0.45).
constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
