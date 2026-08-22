% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate (Composite Overdetermined Reading)
 *   domain: historical sociology/cultural anthropology/legal history
 *
 * SUMMARY:
 *   This story instantiates the composite overdetermined reading of the honor
 *   satisfaction substrate: the regime under which insults among gentlemen
 *   demanded satisfaction through regulated personal combat. The substrate
 *   was a functioning hybrid for most of its life, a real coordination
 *   technology bounding elite violence, carrying a real extraction load of
 *   coerced participation, fatalities, and class-exclusive access to redress,
 *   and it died through two entangled mechanisms: exogenous
 *   legal-institutional suppression and endogenous transformation of the
 *   honor code itself. The reading's distinguishing claim is that these
 *   pathways were not additive and not independent: statutes sat as dead
 *   letters for a century because juries and regiments would not enforce them
 *   against honor's demands, and the cultural turn accelerated precisely when
 *   law began supplying respectable exits (commission-protected refusal,
 *   prosecutable seconds) that reframed refusal as prudence rather than
 *   cowardice. Constraint family note: the colloquial label 'why did dueling
 *   disappear' decomposes into three structurally distinct claims with
 *   different epsilon assessments over the same referent; this file carries
 *   the composite reading, and the monocausal siblings are separate
 *   constraints linked through the network. KEY AGENTS (by structural
 *   relationship): - honor_community_elites: primary beneficiary
 *   (powerful/identity_locked) — collects bounded violence, status
 *   distinction, internal order - coerced_duel_participants: primary target
 *   (moderate/trapped) — bears lethal risk and coerced participation -
 *   military_officer_corps: dual-positioned payer/beneficiary
 *   (organized/constrained) — institutional order gained, bodies risked -
 *   code_custodians_and_seconds: agenda setter (organized/identity_locked) —
 *   administers and interprets the challenge protocol -
 *   commoner_classes_without_standing: excluded class (powerless/trapped) —
 *   bears externalities, holds no standing - state_legal_apparatus:
 *   inter-institutional observer turned executioner
 *   (institutional/analytical) - dissenting_clergy_and_reformers: organized
 *   resistance seat (organized/mobile)
 *
 * KEY AGENTS:
 *   - honor_community_elites: primary beneficiary (powerful/identity_locked) — the gentry and grandees whose disputes the code ordered; receives bounded violence and status distinction; exit means forfeiting the standing that constitutes them
 *   - coerced_duel_participants: primary target (moderate/trapped) — gentlemen compelled to the field by reputation machinery; refusal is social death, acceptance risks physical death
 *   - military_officer_corps: dual payer/beneficiary (organized/constrained) — the corps gains an internal discipline mechanism; individual officers bear the risk under a pincer of regimental penalty and mortal hazard
 *   - code_custodians_and_seconds: agenda setter (organized/identity_locked) — code publishers, tribunal members, and seconds who administer the protocol; service is as compulsory as fighting
 *   - commoner_classes_without_standing: excluded (powerless/trapped) — no honorable redress channel, absorbs spill-over violence, objection structurally inadmissible
 *   - state_legal_apparatus: observer turned executioner (institutional/analytical) — centuries of dead-letter statutes, then enforcement that worked only as elite shielding collapsed
 *   - dissenting_clergy_and_reformers: resistance seat (organized/mobile) — denominational condemnation, burial denial, and the public campaigns that made refusal respectable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.26).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.18).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.76).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical sociology/cultural anthropology/legal history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, 'b2fa07f5-d5da-45db-96b9-d92f7be0bb4b').
narrative_ontology:cs_kernel_codification('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', formalized).
narrative_ontology:cs_authority_grounding('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', lineage).
narrative_ontology:cs_interpretation_layer_present('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b').
narrative_ontology:cs_reading_relation('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', foundational, decline_requires_entangled_dual_causation).
narrative_ontology:cs_axiom_status(decline_requires_entangled_dual_causation, holdable).
narrative_ontology:cs_axiom_grounding('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', decline_requires_entangled_dual_causation, empirically_contingent).
narrative_ontology:cs_axiom('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', secondary, legal_dead_letter_period_evidences_cultural_precondition).
narrative_ontology:cs_axiom_status(legal_dead_letter_period_evidences_cultural_precondition, holdable).
narrative_ontology:cs_axiom_grounding('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', legal_dead_letter_period_evidences_cultural_precondition, empirically_contingent).
narrative_ontology:cs_reference_frame('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', regulated_satisfaction_regime).
narrative_ontology:cs_drift_state('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', fin_de_siecle_residue, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('b2fa07f5-d5da-45db-96b9-d92f7be0bb4b', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_community_elites).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, coerced_duel_participants).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, commoner_classes_without_standing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, military_officer_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The landed gentry, politicians, and senior officers whose disputes the code ordered. They receive bounded violence (quarrels channeled into single regulated encounters instead of feuds), a status marker separating gentlemen from others, and an internal order premised on the equality of members. Leaving the honor community meant forfeiting the standing that constituted their social existence, so exit was not a live option from inside.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_community_elites, beneficiary,
    powerful, generational, identity_locked, national).

% Officers were the densest dueling population. The code gave the corps an internal discipline mechanism that kept quarrels from destroying command cohesion, but individual officers bore the lethal risk directly and faced a pincer: refuse a challenge and face regimental ruin or, in some regimes, formal penalty; accept and face death, wound, or prosecution. Commission structures made resignation costly, so both doors out were gated.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, military_officer_corps, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, military_officer_corps, beneficiary).

% Civilian gentlemen compelled into the field by the machinery of reputation: a refused challenge meant posting as a coward, exclusion from society, and professional ruin, while acceptance meant a real chance of death over a phrase at dinner. Their situation was a trap with two exits, one social death and one physical. Widows and children of the killed absorbed the terminal cost.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, coerced_duel_participants, payer,
    moderate, biographical, trapped, national).

% Publishers and expounders of the written codes duello, members of tribunals of honor, and the seconds who negotiated terms, arranged ground and surgeons, and certified satisfactory outcomes. They administered the challenge protocol and interpreted its edge cases. Refusing to serve as a second carried the same dishonor as refusing a challenge, so their administrative position was as compulsory as the principals'.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, code_custodians_and_seconds, agenda_setter,
    organized, biographical, identity_locked, national).

% Laboring classes, servants, and tradesmen had no standing in the code: when insulted by a gentleman they had no honorable redress channel at all, and when elites dueled over quarrels that began as abuse of dependents, the violence spilled onto commons and streets they inhabited. Their objection, that legitimate violence and honorable satisfaction were reserved for gentlemen, never entered the councils where the code was defended or amended.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, commoner_classes_without_standing, excluded,
    powerless, generational, trapped, national).

% Legislatures, courts, coroners, and military tribunals prohibited dueling by statute for centuries with near-total ineffectiveness, then progressively enforced: coroners' juries returned willful-murder verdicts, survivors and seconds were indicted, commissions were forfeited under army and navy regulations. Their enforcement succeeded only insofar as elite opinion stopped shielding duelists, which made the state's seat dependent on a cultural movement it did not control.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus, observer,
    institutional, generational, analytical, national).

% Church courts, evangelical movements, and anti-dueling associations condemned the practice from outside the honor community, denied Christian burial to the killed, and ran the public-opinion campaigns that made refusal respectable. They bore no costs from the code and collected no rents from it; their seat is the organized resistance the constraint met continuously across the interval.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dissenting_clergy_and_reformers, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, honor_community_elites).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Among a class that carried weapons, treated personal honor as capital, and distrusted or predated reliable courts, the code duello converted potentially unlimited feud, ambush, and vendetta into bounded, consensual, rule-governed encounters: demand for apology, seconds' mediation, retreat-with-honor ramps, and a single observed engagement in place of open-ended retaliation.
% TRANSFER_FUNCTION: Moves lethal risk and reputational capital: the bodies and lives of disputants are pledged as the settlement currency; refusers transfer their standing to challengers by default; dispute-resolution authority moves from courts and communities to the honor community's own tribunals; and status assurance flows to participants at the price of denying any legitimate satisfaction channel to non-gentlemen.
% ABSENT_VOICES: The commoner classes had no standing in the code and no seat in its tribunals; their objection that honorable redress was a gentlemen's monopoly was structurally inadmissible. The killed are the other absent voice: the fatalities whose accumulated weight eventually moved opinion were never present to testify in the forums where satisfaction was debated.
% DISAPPEARANCE_RATIONALE: While the substrate operated, arrangements depended on it: the reputation economy, the challenge-refusal machinery, the seconds' profession, and the tribunals all presuppose it. Had it vanished overnight at its height, elite dispute resolution would have rearranged toward feud, litigation, and social ruin; after its actual dissolution the rearrangement was slower but real, as displaced quarrels migrated into courts, newspapers, and political channels.
% FOUNDING_PROBLEM: How a weapon-bearing status class could settle insults without descending into endless retaliatory feud, given that its members regarded personal courage as the foundation of their standing and regarded recourse to law over words as dishonorable.
% FOUNDING_PROBLEM_CORROBORATION: No corroboration comes from inside the beneficiary set alone: state legal records (statutes, coroners' inquests, courts-martial proceedings), ecclesiastical condemnation registers, anti-dueling society publications, and the modern historiography (Kiernan, Freeman, McCartney) all attest the founding problem and its progressive dissolution from outside the honor community. The honor community itself attested the opposite, that satisfaction remained necessary, which is precisely the contest the kernel records.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).
:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as an inverted-U peaking mid-interval (0.70 at t=40): while belief was strong, extraction was masked inside consent; as belief wavered but compulsion held, the men in the field were fighting without conviction, which is extraction in its naked form. Suppression_requirement is tracked because enforcement-capacity change IS this story: the series rises to t=40 (the ratchet, courts-martial punishing refusal as conviction eroded) and then falls steeply as the state criminalized the constraint's own coercive apparatus, indicting seconds and returning murder verdicts. That inverted-U is the entanglement made visible: the rise registers endogenous collapse (compulsion replacing conviction), the fall registers exogenous dismantling, and neither segment is readable without the other. Theater_ratio climbs monotonically from 0.15 to 0.76 as delopement, wide-fired pistols, pre-arranged surgeons, and finally purely ceremonial affairs replaced function with form, classic Goodhart drift of the satisfaction form outliving the satisfaction substance. Accessibility_collapse is 0.70: inside the honor frame alternatives collapsed almost completely (an insult unanswered was indelible), but the frame itself admitted negotiated apology ramps, so not the near-total collapse of a natural law. Resistance is 0.60: continuous, organized, multi-century resistance from church, law, and reform press, the signature of a construct that had to be defended. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. All three series run on one shared eight-point grid so every metric is asserted at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structure. From the honor elites' seat the substrate is civilization itself: the perceived extraction is near zero because the costs fall on those who consent within the identity frame, and the arrangement reads as a rope they built. From the coerced participants' seat the same structure is a trap with two mortal exits, computing as heavy extraction with no arbitrage. From the officer corps' seat it is genuinely dual: institutional benefit and personal exposure in the same body. From the state's seat the constraint migrates across the interval from tolerable nuisance to intolerable affront to the legal monopoly on violence, which is why the same statute that was a dead letter in 1750 was an instrument of destruction by 1850. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. honor_community_elites sit near the beneficiary pole (d near 0.05-0.15): subsidized by bounded violence and status rents, identity-locked so no exit damping applies. code_custodians_and_seconds take small administrative rents (d near 0.2) but are identity-bound to the machinery they run. military_officer_corps are genuinely dual-positioned (d near 0.5): institutional subsidy and personal exposure in one seat, which is why they carry secondary_role beneficiary. coerced_duel_participants sit near the full-target pole (d near 0.9): trapped between social and physical death, no arbitrage, identity pressure on top of structural compulsion. commoner_classes_without_standing carry high d (near 0.7) through exclusion and externality rather than direct compulsion. The state and clergy seats are analytical/resistance positions collecting nothing. No directionality overrides are used: the beneficiary/victim declarations plus differentiated exit atoms already separate every seat the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, bounding elite violence among men who disdained courts, was progressively dissolved rather than transferred: no successor institution inherited the extraction, and the residue (ceremonial affairs, academic fencing scars) maintains the form without the function. The composite reading is what prevents mandatrophy misdiagnosis in both directions. Read as pure rope-breaking (the practice_decline sibling), the decline looks like a working tool cut by an external hand, hiding that the tool's users had already stopped believing in it. Read as pure mountain erosion (the cultural_contraction sibling), the decline looks like a natural attitude fading on its own schedule, hiding that statutes, prosecutions, and commission regulations were constitutive mechanisms of the cultural shift, not background noise. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the founding problem's death is disputed across the kernel's readings, so no zombie flag fires, but the contested status itself is the signal that the genealogy is the live question. Terminal drift toward piton-like residue (high theater, low extraction, inertial survival in closed subcultures) is documented in the measurement series and flagged in the residue omega rather than forced into the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the composite overdetermined account the correct instantiation of the honor_satisfaction_substrate kernel, or does one of the monocausal siblings (practice_decline, cultural_contraction) capture the actual structure?',
    'Comparative analysis of jurisdictions and periods where one lever moved without the other: France (legal tolerance, cultural persistence into the twentieth century), Britain (prohibition preceding cultural death by decades), Germany (Mensur flourishing under formal prohibition). If single-lever variation predicts outcomes, a monocausal sibling wins; if outcomes track only joint movement, the composite stands.',
    'Determines whether this story stands as one entangled constraint or decomposes into two linked stories (exogenous-suppression and endogenous-delegitimation) with separate epsilon values and separate beneficiary structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the composite reading or a monocausal sibling correctly instantiates the kernel.').

omega_variable(
    causal_pathway_entanglement,
    'Which direction did the non-independence run: law working through culture (prosecutions and murder verdicts stigmatizing the practice and accelerating delegitimation), or culture working through law (elite opinion softening before enforcement became possible)?',
    'Sequence analysis of prosecution outcomes against elite opinion indicators: coroners'' jury verdicts, jury acquittal rates in dueling trials, regimental enforcement records, and press framing, tested for lead-lag ordering.',
    'Reallocates causal weight within the transfer_function and determines whether exogenous suppression is constitutive of the endogenous shift or merely catalytic; changes the entanglement''s direction without changing its existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_entanglement, empirical, 'Direction of the causal coupling between legal suppression and cultural delegitimation.').

omega_variable(
    honor_naturalness_ambiguity,
    'Was the satisfaction requirement held by its practitioners as natural law (an inevitable condition of gentility, alternative-less) or as constructed convention (a chosen and maintainable code)?',
    'Close reading of honor-community discourse: defenders who argued impossibility-of-alternative speak in the mountain register; defenders who argued tradition, choice, and codification speak in the construct register. The published codes duello are themselves evidence of construction.',
    'If experienced as natural law, the substrate enjoyed mountain immunity in perception, explaining why centuries of statutes stayed dead letters; if constructed, collapse was structurally available throughout and the enforcement record reads as ordinary politics. Affects the accessibility_collapse interpretation and the kernel''s mountain-erosion component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honor_naturalness_ambiguity, conceptual, 'Natural-law versus constructed-convention framing of the satisfaction requirement.').

omega_variable(
    coercion_mechanism_composition,
    'Was participant coercion primarily structural (posting, ostracism, professional ruin, court-martial) or internalized (identity fusion making refusal literally unthinkable before any sanction applied)?',
    'Trajectories of documented refusers: whether predicted sanctions actually materialized (structural coercion) or whether anticipatory shame and self-conception did the work before any sanction could (internalized). Post-collapse persistence of honor anxiety in veterans of the code is the cleanest internalization signal.',
    'Splits the suppression attribution between machinery and identity; shifts coerced_duel_participants between trapped and identity_locked exit readings, which changes their computed effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_composition, empirical, 'Structural versus internalized composition of the coercion holding participants in the field.').

omega_variable(
    residue_function_status,
    'Does the terminal residue (academic Mensur, ceremonial first-blood affairs, scar-collecting) retain any genuine satisfaction function, or is it wholly theatrical maintenance of a form whose function has atrophied?',
    'Participant testimony and injury data from residue-era practices: whether disputes are actually settled by the encounter or whether the encounter merely performs settlement that negotiation already achieved.',
    'If wholly theatrical, the constraint''s terminal state warrants piton refinement (inertial, administrator-could-change-it, cost-asymmetry profile); if locally functional, it remains tangled-rope residue and the classification holds to interval end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residue_function_status, empirical, 'Functional versus theatrical status of the dueling residue at interval end.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hss_composite_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hss_composite_tr_t20, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hss_composite_tr_t40, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(hss_composite_tr_t60, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(hss_composite_tr_t80, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(hss_composite_tr_t100, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 100, 0.58).
narrative_ontology:measurement(hss_composite_tr_t120, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 120, 0.68).
narrative_ontology:measurement(hss_composite_tr_t140, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 140, 0.76).

% Extraction over time
narrative_ontology:measurement(hss_composite_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(hss_composite_be_t20, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(hss_composite_be_t40, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(hss_composite_be_t60, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(hss_composite_be_t80, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(hss_composite_be_t100, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(hss_composite_be_t120, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 120, 0.34).
narrative_ontology:measurement(hss_composite_be_t140, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 140, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(hss_composite_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hss_composite_su_t20, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(hss_composite_su_t40, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(hss_composite_su_t60, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(hss_composite_su_t80, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(hss_composite_su_t100, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 100, 0.36).
narrative_ontology:measurement(hss_composite_su_t120, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 120, 0.27).
narrative_ontology:measurement(hss_composite_su_t140, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 140, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the disappearance of dueling'. The label conflates three structurally distinct claims: (1) practice_decline_reading, a persistent normative substrate broken by exogenous enforcement; (2) cultural_contraction_reading, a transformed substrate in which the practice became unthinkable; (3) this composite_overdetermined_reading, in which both mechanisms operated simultaneously through non-independent pathways. Each member carries its own epsilon over the shared referent (the standing satisfaction arrangement), assessed by that reading's own lights; the readings are linked through affects_constraints rather than merged, per the epsilon-invariance principle. The upstream/downstream pressure runs from this composite reading toward both siblings: its dead-letter-statute evidence pressures the sufficiency claim of the exogenous account, and its prosecution-opinion timing correlations pressure the sufficiency claim of the endogenous account.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
