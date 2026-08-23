% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical sociology / legal anthropology / commitment systems
 *
 * SUMMARY:
 *   Between the late eighteenth and early twentieth centuries the governing
 *   classes of Britain, North America, and much of continental Europe rebuilt
 *   the meaning of honor from demonstrated readiness for private violence to
 *   demonstrated self-command. Statutes against dueling had existed for
 *   centuries and failed; what changed was the concept that made refusal
 *   shameful. Once honor meant conscience, credit, and character, issuing a
 *   challenge marked the challenger as uncivilized rather than the refuser as
 *   coward, and the practice died not because penalties rose but because its
 *   payoff in standing vanished. The eps referent for this story is the
 *   standing redefined-honor arrangement itself, assessed by this reading's
 *   own lights: a moral order that runs largely without enforcement
 *   machinery. KEY AGENTS (by structural relationship): see key_agents; the
 *   arrangement's parties span the reform bloc that administered the
 *   redefinition, the institutional and class seats that collected its
 *   benefits, the holdout seats whose status capital it devalued, and the
 *   excluded and analytical seats at its edges.
 *
 * KEY AGENTS:
 *   - - evangelical_and_moral_reformers: Agenda-setting reform bloc (organized/mobile) — administers the redefinition of honor through preaching, tract, and pledge
 *   - - military_establishment: Institutional administrator turned beneficiary (institutional/constrained) — codifies abolition once the culture carries it
 *   - - young_officers: Primary intended beneficiary (moderate/constrained) — the class the old code killed; freed only when the class moved together
 *   - - middle_class_professionals: Secondary beneficiary (organized/mobile) — buys gentility at the new, cheaper definition
 *   - - martial_aristocracy_holdouts: Primary payer (powerful/identity_locked) — status capital denominated in the old code
 *   - - southern_american_gentry: Regional payer (powerful/identity_locked) — last large constituency of the challenge
 *   - - dueling_specialists: Incidental payers (moderate/constrained) — seconds and masters whose trade the redefinition abolished
 *   - - anti_dueling_women_organizers: Excluded voice (organized/trapped) — campaigned against the duel from outside the forums that set honor's meaning
 *   - - historical_sociologists: Analytical observer (analytical/analytical) — sees the full structure retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.13).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.08).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.07).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.13).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.07).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical sociology / legal anthropology / commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '2a6ecb07-7633-49fa-aa3f-6baa6e310c7a').
narrative_ontology:cs_kernel_codification('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', distributed).
narrative_ontology:cs_authority_grounding('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', practice).
narrative_ontology:cs_interpretation_layer_present('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a').
narrative_ontology:cs_reading_relation('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', foundational, honor_consists_in_self_command_not_readiness_for_violence).
narrative_ontology:cs_axiom_status(honor_consists_in_self_command_not_readiness_for_violence, holdable).
narrative_ontology:cs_axiom_grounding('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', honor_consists_in_self_command_not_readiness_for_violence, deontological).
narrative_ontology:cs_axiom('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', secondary, private_vengeance_usurps_public_dispute_authority).
narrative_ontology:cs_axiom_status(private_vengeance_usurps_public_dispute_authority, holdable).
narrative_ontology:cs_axiom_grounding('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', private_vengeance_usurps_public_dispute_authority, conventional).
narrative_ontology:cs_reference_frame('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', honor_as_moral_character_incompatible_with_private_violence).
narrative_ontology:cs_drift_state('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', contemporary_mass_society, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('2a6ecb07-7633-49fa-aa3f-6baa6e310c7a', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, young_officers).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, middle_class_professionals).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, military_establishment).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, martial_aristocracy_holdouts).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, southern_american_gentry).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, dueling_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergymen, tract writers, and moral-society organizers who spent the late eighteenth and nineteenth centuries arguing that a gentleman's worth lay in conscience and self-command rather than readiness to fight. They preached against the duel, ran pledge campaigns, petitioned regiments and universities, and supplied the vocabulary — cowardice recast as sin, forbearance recast as courage — that gradually made the challenge reply dishonorable. They commanded no police; their instrument was meaning.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, evangelical_and_moral_reformers, agenda_setter,
    organized, generational, mobile, national).

% Army and navy hierarchies that alternately prosecuted duelists and quietly tolerated the custom for generations, then codified its abolition through articles of war amendments, academy regulations, and courts of inquiry that converted affairs of honor into administrative matters. By the twentieth century an officer treating an insult as a dueling matter risked ridicule rather than respect. They gained an officer corps that stopped shooting itself and a disciplinary system with fewer private jurisdictions inside it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, military_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, military_establishment, beneficiary).

% Subalterns and junior officers, the men the old code killed most often. Before the redefinition, refusing a challenge meant social death and accepting it meant literal death, with mess tables and promotion boards watching. After it, a junior man could laugh off an insult, refer the matter to superiors or the press, and lose nothing. Their relief was never individually choosable — it arrived only when the class moved together.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, young_officers, beneficiary,
    moderate, biographical, constrained, national).

% Lawyers, physicians, merchants, and clergy rising into genteel standing across the nineteenth century. Their claim to honor rested on credentials, conduct, and credit rather than swordsmanship; a code tying gentility to private violence priced them out of full standing. The redefinition admitted them: once honor meant character they qualified by default, and the men who insisted on the older test looked provincial.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, middle_class_professionals, beneficiary,
    organized, generational, mobile, national).

% Older landed and officer families whose distinction had been banked in the martial code — ancestors who had fought, sons trained to answer for the name. As the meaning shifted, their inheritance depreciated: the posture that had signaled rank now signaled boorishness, and the invitations went to men who could not have loaded a pistol under pressure. Leaving the old code meant disowning the family's accumulated capital; keeping it meant shrinking into caricature.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, martial_aristocracy_holdouts, payer,
    powerful, biographical, identity_locked, national).

% Plantation-region gentlemen in the United States who kept the challenge alive latest and longest, treating personal sovereignty as inseparable from mastery. They paid in national reputation — northern newspapers read their affairs of honor as proof of backwardness — and eventually in isolation, as railroads, professions, and churches tied the region into an honor economy that no longer traded in pistol answers.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, southern_american_gentry, payer,
    powerful, biographical, identity_locked, regional).

% Seconds, attending surgeons, and fencing masters whose livelihoods rode on the custom. Seconds faced prosecution as principals; masters watched the dueling trade contract into sport fencing and salles d'armes teaching exercise rather than satisfaction. A few reinvented themselves as athletic instructors; the rest aged out of a profession their clients' grandchildren considered barbaric.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, dueling_specialists, payer,
    moderate, biographical, constrained, regional).

% Widows, mothers, and reform women who campaigned against the duel — petitioning, shaming, memorializing the dead — while sitting outside every forum where honor's meaning was actually renegotiated: the mess, the club, the regimental court, Parliament. Their testimony entered the record as sentiment; the men who redrew the code cited it and did not seat it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, anti_dueling_women_organizers, excluded,
    organized, generational, trapped, national).

% Later analysts of the transition who reconstructed the decline from coroners' records, regimental courts, pamphlet wars, and family papers. They observe the whole structure from outside it and disagree among themselves about which lever moved it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Escapes the challenge dilemma: no gentleman could unilaterally refuse a challenge without ruin, so the class redefined honor collectively so that refusal costs nothing and issuing a challenge marks the issuer. It also standardizes grievance handling — apology protocols, seconds' negotiation, courts of inquiry, public correction — replacing private war with shared procedure.
% TRANSFER_FUNCTION: Moves definitional authority over honor from the martial estate to clerical, professional, and bureaucratic elites; moves dispute-resolution business from private grounds to public and administrative fora; and converts a private liability for violent death into a class-wide assurance of bodily security for elite young men.
% ABSENT_VOICES: The men killed under the old code cannot testify; their deaths survive as coroners' records and family papers. Anti-dueling women organizers campaigned from outside the messes, clubs, and legislatures where honor's meaning was renegotiated — cited, rarely seated. Enlisted ranks never held the honor-standing the code regulated and had no voice in its revision.
% DISAPPEARANCE_RATIONALE: If the redefinition snapped back overnight — if honor again entailed answering insults with force — challenge obligations would revive first in military academies and officer messes, then in politics and the press; courts and apology protocols would lose their grip on affairs of honor; mortality among elite young men would climb back toward early nineteenth-century rates. The arrangement is load-bearing, not decorative.
% FOUNDING_PROBLEM: An honor code that obligated gentlemen — above all officers — to answer insult with lethal private combat, killing more of certain peacetime officer corps than war did, and defeating three centuries of statutory prohibition because the law punished the act while leaving the concept intact.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: historical-demographic work documenting duel mortality from coroners' inquests and regimental casualty returns, and comparative scholarship on honor cultures lacking the redefinition — street honor codes, pre-reform militaries — where status disputes still escalate lethally. Both bodies attest that the underlying problem, elite status competition carrying lethal escalation risk, remains live wherever the redefinition has not taken.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.13, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state extractiveness is low (0.13): the residual costs — holdout stigma, the specialists' abolished trade — are real but small and fading. Suppression is lower still (0.08) and this is the reading's signature: legal penalties predated the decline by centuries and failed, so the operative suppression at end-state is whatever social ridicule survives, which is nearly nothing. Theater is minimal (0.07) because the function is intact and performed daily without ceremony — mess norms, apology protocols, courts of inquiry process grievances invisibly. Accessibility_collapse is moderate (0.55): once the redefinition is understood, dueling is not merely costly but meaningless, yet the collapse applies to one practice while ordinary dispute alternatives (courts, press, public correction) multiplied. Resistance (0.18) reflects documented pockets — ceremonial duels, literary nostalgia, surviving Mensur fraternities — that never reconstituted a constituency. The measurement series share one grid (1780/1815/1845/1875/1900/1920). Extractiveness follows a hump, not a ratchet: it peaks mid-transition (0.33 at 1845) when the emerging norm actively stigmatizes holdouts, then decays as the old code's bearers die out. Suppression_requirement falls monotonically (0.58 to 0.08) — enforcement decay through normalization, the exact trajectory the contraction claim predicts: the arrangement needed courts-martial and pulpits while it contradicted living practice, and needed almost nothing once practice and meaning aligned. Theater bumps mildly during the organized-campaign decades (pledge societies and subscription lists had performative membership) and subsides.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the young_officer seat the arrangement is emancipation: the same insult that once forced a choice between social death and literal death now costs a shrug. From the martial_aristocracy_holdout seat the same arrangement is dispossession: a life's training and a family's accumulated standing reclassified as boorishness overnight. From the reformer and military-administrator seats it is a completed reform requiring only maintenance. The engine derives these divergences from the declared roles, power atoms, and exit options; the divergence between the identity_locked payers and the mobile beneficiaries is the largest gap in the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: young_officers, middle_class_professionals, and military_establishment sit near the beneficiary end (low d, subsidized or damped effective extraction), with the professionals' mobility pushing them furthest toward arbitrage-grade position. Victim declarations push the three payer seats toward the target end, and the identity_locked exit of the two holdout seats places them nearest full-target: they cannot leave the arrangement without disowning themselves. dueling_specialists are targets with constrained but real exit (sport-fencing pivots), moderating their d slightly below the holdouts. The excluded and observer seats sit outside the extraction arithmetic. No directionality overrides were needed: the beneficiary/victim plus exit data already separate the seats correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — elite status competition with lethal escalation risk — is perennial, and the arrangement still addresses it, so nothing here has outlived its function and mandatrophy is not resolved. The classification discipline guards both adjacent errors. Reading the anti-dueling norm as pure coordination ignores that its transition phase genuinely extracted from identifiable payers (the holdouts and specialists) — a snare-flavored misread of the mid-interval data. Conversely, reading the holdout persecution as the essence of the arrangement mistakes a fading transition cost for the structure's function and misses that no seat captures gains: receipts are diffuse, the function is intact, and the extraction decays rather than accumulates. The hump-shaped extractiveness series is the evidence that separates transient coordination friction from a durable extraction mechanism — and the transience itself is flagged as an open omega rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the contraction_reading of the honor_violence_legitimacy kernel; what would change structurally if the drop_reading or composite_reading were instantiated instead?',
    'Author the sibling stories as separate epsilon-invariant constraints and compare extractiveness, suppression, and stakeholder exit profiles across the family; arbitration rests on which profile the enforcement and mortality records fit.',
    'The drop_reading would author high suppression sustained by legal and economic penalty with persistence-by-coercion; the composite_reading would split causal weight and sit between. This reading''s low suppression and self-enforcement profile are indexical to the contraction claim, not properties of the bare historical label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of the honor-violence kernel.').

omega_variable(
    suppression_attribution_ambiguity,
    'Is the low suppression real conceptual self-enforcement, or an artifact of crediting to meaning what law and economy accomplished?',
    'Compare jurisdictions with similar legal penalties but divergent honor ideologies — the British army after the 1844 regulations versus German student corps retaining the Mensur under comparable legal exposure; if outcomes diverge where penalties match, meaning carried the load.',
    'If external costs did the work, this reading understates suppression and the arrangement computes nearer a coercion-sustained profile with higher effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_attribution_ambiguity, empirical, 'Whether enforcement decay reflects conceptual self-enforcement or misattributed external costs.').

omega_variable(
    holdout_identity_lock_direction,
    'Do the holdout payers bear the arrangement''s costs because the new honor regime traps them, or because their own prior code binds them from inside?',
    'Biographical study of holdout families across the transition: whether abandoning the old code was materially penalized by the new regime after consolidation, or only self-penalized.',
    'If self-binding, their directionality sits nearer symmetric and the arrangement''s residual extraction shrinks further; if externally penalized, effective extraction on that seat is understated by the current declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holdout_identity_lock_direction, conceptual, 'Source of holdout persistence: external trap or internal code.').

omega_variable(
    transition_extraction_transience,
    'Was the mid-interval rise in extraction — the stigmatization and exclusion of holdouts — a transient coordination cost, or the first instance of a recurring pattern in which each redefinition of honor exiles the previous definition''s bearers?',
    'Track subsequent honor redefinitions — professionalization, technocratic meritocracy — for repeated exile dynamics against displaced status elites.',
    'If recurring, the arrangement carries a structural extraction ratchet the end-state metrics miss; if transient, the hump is transition friction around a genuinely low-extraction coordination order.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_extraction_transience, empirical, 'Whether definitional transitions systematically extract from displaced elites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1780, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__contraction_reading, theater_ratio, 1780, 0.09).
narrative_ontology:measurement(hono_tr_t1815, honor_violence_legitimacy__contraction_reading, theater_ratio, 1815, 0.19).
narrative_ontology:measurement(hono_tr_t1845, honor_violence_legitimacy__contraction_reading, theater_ratio, 1845, 0.23).
narrative_ontology:measurement(hono_tr_t1875, honor_violence_legitimacy__contraction_reading, theater_ratio, 1875, 0.17).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.11).
narrative_ontology:measurement(hono_tr_t1920, honor_violence_legitimacy__contraction_reading, theater_ratio, 1920, 0.07).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1780, 0.16).
narrative_ontology:measurement(hono_be_t1815, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1815, 0.27).
narrative_ontology:measurement(hono_be_t1845, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1845, 0.33).
narrative_ontology:measurement(hono_be_t1875, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1875, 0.24).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.17).
narrative_ontology:measurement(hono_be_t1920, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1920, 0.13).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1780, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1780, 0.58).
narrative_ontology:measurement(hono_su_t1815, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1815, 0.52).
narrative_ontology:measurement(hono_su_t1845, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1845, 0.41).
narrative_ontology:measurement(hono_su_t1875, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1875, 0.27).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(hono_su_t1920, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1920, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the decline of dueling' decomposes into three epsilon-invariant readings of one kernel, per the decomposition discipline. This file authors the contraction reading: epsilon is assessed over the standing redefined-honor arrangement as this reading sees it — a self-enforcing moral order with negligible residual extraction and decaying enforcement need. The drop_reading authors the same historical referent as a legitimacy-preserving practice suppressed by external cost (high suppression, persistence-by-coercion); the composite_reading splits causal weight and takes intermediate metrics. Each sibling is a separate constraint story with its own beneficiaries, victims, and claimed type; this file links them through affects_constraints and through cs_structure.reading_relations, and the family comparison is the designed instrument for resolving the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
