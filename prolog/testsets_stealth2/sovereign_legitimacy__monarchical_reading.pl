% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Hereditary Sovereign Legitimacy — Monarchical Reading (Divine Right of Kings)
 *   domain: political philosophy/constitutional theory/legitimacy studies
 *
 * SUMMARY:
 *   The arrangement under contest is hereditary sovereignty: legitimate
 *   authority originates in the sovereign's bloodline, is sanctioned by God
 *   at anointing, and descends by fixed succession rule; all other authority
 *   is held at the sovereign's grant. This file instantiates ONLY the
 *   monarchical reading of the sovereign_legitimacy kernel (see
 *   kernel_context); the republican and constitutional-hybrid readings are
 *   separate constraints with separate epsilon, victim sets, and validation
 *   mechanisms. Claim and metrics are independent: the reading itself
 *   presents the arrangement as divinely warranted and natural, while the
 *   authored metrics describe its actual operation — high extraction
 *   concentrated on excluded subjects, heavy suppression of alternative
 *   legitimacy claims, and a real but conditional succession-coordination
 *   function. Structurally I read this as a tangled rope: a genuine
 *   coordination achievement (determinate, publicly verifiable succession)
 *   fused with asymmetric extraction (hereditary monopoly on authority,
 *   aristocratic rents, total exclusion of subjects), held together by active
 *   enforcement.
 *
 * KEY AGENTS:
 *   - hereditary_sovereign_house: agenda-setter and primary beneficiary (institutional/identity_locked) — sets succession law, collects the transfer, cannot abandon the bloodline basis without dissolving its own title
 *   - aristocratic_hierarchy: secondary beneficiary (organized/constrained) — collects rank, land, and exemption inside the descent order
 *   - established_clergy: beneficiary and co-administrator of validation (institutional/identity_locked) — performs the anointing, partly subordinated to the crown it blesses
 *   - political_subjects: primary target (powerless/constrained) — bears taxation, conscription, and total exclusion from authority
 *   - rival_pretender_lines: excluded claimants (organized/trapped) — their claims are voided by definition, the enforcement object itself
 *   - popular_sovereignty_advocates: excluded voices (moderate/mobile) — censored at home, publishing from exile
 *   - legitimacy_theorists: analytical observer (analytical/analytical) — sees the full structure across kingdoms and epochs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.72).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.78).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Hereditary Sovereign Legitimacy — Monarchical Reading (Divine Right of Kings)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political philosophy/constitutional theory/legitimacy studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3').
narrative_ontology:cs_kernel_codification('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', formalized).
narrative_ontology:cs_authority_grounding('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', lineage).
narrative_ontology:cs_interpretation_layer_present('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3').
narrative_ontology:cs_reading_relation('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', sovereign_legitimacy__constitutional_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', foundational, legitimate_authority_descends_by_bloodline_right).
narrative_ontology:cs_axiom_status(legitimate_authority_descends_by_bloodline_right, holdable).
narrative_ontology:cs_axiom_grounding('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', legitimate_authority_descends_by_bloodline_right, theological).
narrative_ontology:cs_axiom('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', secondary, unbroken_succession_validates_reign).
narrative_ontology:cs_axiom_status(unbroken_succession_validates_reign, holdable).
narrative_ontology:cs_axiom_grounding('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', unbroken_succession_validates_reign, conventional).
narrative_ontology:cs_reference_frame('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', divinely_ordained_hereditary_succession).
narrative_ontology:cs_drift_state('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', contemporary_mass_suffrage_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('5ab63693-eccd-404c-8a9f-bfeb3ddc7bc3', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_sovereign_house).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, established_clergy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, political_subjects).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_right_of_kings_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, hereditary_succession_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the crown by right of birth and passes it to the designated heir. Receives oaths of fealty, taxation, and military service; grants titles, offices, and land to supporters. Its entire claim to rule rests on unbroken descent — an adopted heir, an elected successor, or a renounced bloodline would dissolve the house's title, so it cannot change the basis of its own legitimacy without ceasing to be what it is. Marriage alliances tie it to ruling houses across borders.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_sovereign_house, agenda_setter,
    institutional, generational, identity_locked, continental).

% Holds landed estates, hereditary seats, and tax exemptions granted by the crown in exchange for military and administrative service. Collects dues and labor from tenants on its lands. Its rank exists only inside the descent-based order; a shift to elected or purchasable office would strip its titles of meaning. Leaving would mean surrendering estate and status together.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    organized, generational, constrained, national).

% Performs the anointing and coronation that publicly validate each succession and preaches obedience to ordained authority. Receives establishment privileges, tithes, and legal immunities in return. After the jurisdictional settlements it also loses courts and appointments to royal control whenever the crown prevails, so its position mixes privilege with subordination; its doctrine leaves no room for blessing a legitimacy sourced anywhere but the altar.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, established_clergy, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, established_clergy, agenda_setter).

% Pay taxes, serve in armies, and owe obedience without any share in choosing or checking the ruler. Access to office runs through patronage they do not control. Some emigrate or enter religious houses; most remain bound to parish and land. Grievances surface in petitions, riots, and occasionally revolt, each of which the law classes as treason.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, political_subjects, payer,
    powerless, biographical, constrained, national).

% Descend from other branches of royal or formerly royal houses and assert their own succession claims. The prevailing succession law defines their claims as usurpation before any hearing occurs. They raise armies, seek foreign backing, or wait out reigns in exile; their only path back runs through the same descent logic that bars them from peaceful invocation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, rival_pretender_lines, excluded,
    organized, generational, trapped, national).

% Pamphleteers, jurists, and exiles who argue that authority originates in the people and may be withdrawn. Licensing regimes, censorship, and treason prosecutions push them abroad; they publish from foreign presses and correspond across borders. Their arguments circulate even where their persons cannot safely return.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, popular_sovereignty_advocates, excluded,
    moderate, biographical, mobile, continental).

% Analyzes succession disputes, coronation oaths, and constitutional texts across kingdoms without holding a stake in any particular line. Produces the comparative record — patriarchalist apology against contract argument, court theology against revolutionary assembly — that later generations use to adjudicate the dispute.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, legitimacy_theorists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_sovereign_house).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the transfer-of-power problem: a determinate, publicly verifiable answer to who rules next, fixed before the vacancy opens, so that a ruler's death or incapacity does not by itself trigger armed competition. Provides a single point of allegiance for command, justice, and diplomacy.
% TRANSFER_FUNCTION: Moves obedience, taxation, military service, and deference upward from subjects to the crown and its granted hierarchy; moves protection, office, title, land, and sacral legitimation downward from crown and church to grantees.
% ABSENT_VOICES: Subjects have no seat: legitimacy is validated by bloodline and ritual, not consent, so those governed are structurally absent from the conversation that authorizes their governance. Popular-sovereignty advocates are censored or exiled; rival pretender lines are defined as illegitimate rather than heard. The unanimity of the validating chorus (court, pulpit, heraldry) arises largely because dissenting seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the descent-based legitimacy rule vanished overnight, every reigning house's title evaporated simultaneously: succession disputes would open in every realm at once, aristocratic rank and estate law built on crown grant would lose their foundation, church establishment would lose its counterparty, and treason law would have nothing left to protect. The political map would reorganize around whatever replacement legitimacy claim each polity could mount.
% FOUNDING_PROBLEM: After the fragmentation of the post-imperial West, the problem was establishing durable, undisputed command authority — coordinating defense, justice, and succession across territories where kinship, conquest, and election each produced recurring armed contests over who should rule.
% FOUNDING_PROBLEM_CORROBORATION: Historians of medieval state formation corroborate the founding problem from outside the benefiting parties: the documentary record of private war, failed elections, and the peace-of-God movements attests the disorder the arrangement addressed. On the other side, constitutional theorists, revolutionary assemblies, and the legislative record of states that replaced hereditary succession with electoral or constitutional mechanisms corroborate from outside that the problem has been solvable by other means for centuries. Subject-class attestation enters the record almost exclusively through revolt documents, which cuts both ways and is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.72: taxation, conscription, and exclusion from office are borne by subjects while authority and its fruits concentrate in one descent line; the reading's own lights credit the protection and order flowing back down, which holds epsilon below the pure-snare range despite the severity of the transfer. Suppression 0.78: persistence requires treating rival claims as usurpation and popular-sovereignty argument as treason — licensing, censorship, prosecution — not participant preference; suppression is authored as a raw structural property and is not scaled by power or scope. Theater 0.42: anointing and court ritual genuinely perform the public validation the mechanism needs, but a growing share of ceremonial activity legitimates rather than governs as administrative power migrates to councils and bureaucracies. Accessibility collapse 0.60: within a mature monarchical order the alternative — authority originating from below — is close to unthinkable for most subjects, yet pretender lines and printed argument keep rival claims partly alive, so alternatives do not fully collapse. Resistance 0.58: revolt, regicide, and revolutionary episodes recur across the interval. All three tracked metrics run on one shared seven-point grid (t=0..240) so every metric is authored at every examined point. Suppression_requirement is authored deliberately: the story specifically traces enforcement-capacity growth — print culture spreads alternative legitimacy claims and the machinery of licensing and treason prosecution hardens in response — which is an enforcement-ratchet dynamic, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign-house seat the arrangement is the divinely ordered constitution of society itself: that seat experiences order, sacral duty, and continuity, not extraction. From the subject seat the same structure is taxation without voice and obedience without recourse. Aristocrats experience guaranteed rank; pretenders experience a door defined as no door; clergy experience privilege braided with subordination. Identity-lock dynamics bind the two locked seats: the dynasty's lock is institutional identity fusion — the house has become its bloodline claim, so adopting election or consent dissolves the title rather than reforming it — while the clergy's lock is doctrinal, its liturgy unable to bless a legitimacy sourced outside the altar. Were either frame to break, the seat's classification would shift sharply: a crown that accepted constitutional mediation becomes a hybrid-reading stakeholder overnight. The engine computes these divergent per-seat classifications from the structural data; the gap between the reading's self-presentation and the payer-seat experience is the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign house sits at the beneficiary pole: it collects obedience, treasure, and validation and controls the rules (d near 0). Aristocracy and established clergy collect rank, land, tithe, and privilege without running the succession machinery — low derived d. Political subjects sit at the target pole: they bear the full transfer with constrained exit (d near 1). Two overrides correct derivations the structural declarations alone would get wrong. First, established_clergy derives near-full-beneficiary from its privileges, but after the investiture settlements the crown disciplines appointments and courts, making the clergy partly an instrument of the arrangement it blesses — overridden to d=0.22. Second, rival_pretender_lines carry no beneficiary or victim declaration and would otherwise take a canonical fallback, yet their actual position is squarely targeted: the enforcement machinery exists to void precisely their claims — overridden to d=0.85. Popular-sovereignty advocates are likewise excluded rather than coordinated; their mobility (foreign publication) moderates but does not invert their target-side exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two opposite mislabels. Reading the arrangement as pure snare erases the real coordination achievement — determinate succession repeatedly averted the armed free-for-all that contested transfers produced where no rule held — and would predict collapse on liberalization that the surviving hybrid monarchies falsify. Reading it as pure rope erases the asymmetric extraction — hereditary monopoly, aristocratic rents, total exclusion — and would treat the suppression statistics as incidental overhead. The founding problem (post-fragmentary disorder requiring undisputed command) is contested rather than dead: defenders attest its persistence wherever legitimacy is disputed, while constitutional theorists attest that alternative mechanisms solved it long ago. Because the status is contested rather than dead, the mismatch consumer should not fire the zombie flag on this story; however, the steadily rising theater_ratio series marks exactly where drift toward performance would register if the founding problem died outright — the arrangement's ceremonial share grows as its governing substance migrates to councils, parliaments, and bureaucracies it does not control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the monarchical reading of the sovereign_legitimacy kernel; what would the republican_reading or constitutional_hybrid_reading change structurally if instantiated instead?',
    'Author the sibling stories and compare victim sets, epsilon, and validation mechanisms. The disagreement is located at the source and direction of legitimate authority: downward from bloodline (this reading) versus upward from popular consent versus dual-sourced with constitutional mediation.',
    'Under the republican reading the victim set expands to include all unelected authority holders and epsilon is authored higher; under the hybrid reading the victim set shrinks to holders of residual prerogative and suppression drops as constitutional mediation absorbs drift without suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this constraint is one of three readings of the sovereign_legitimacy kernel.').

omega_variable(
    divine_sanction_warrant_status,
    'Is the divine-sanction warrant load-bearing for the arrangement''s operation, or decorative once succession statute fixes the heir?',
    'Compare reigns where the theological warrant was publicly contested (trial and execution of a king, expulsion of a dynasty) against reigns where it went unchallenged; test whether enforcement intensity tracks theological challenge or mere statutory ambiguity.',
    'If decorative, the arrangement reduces to conventional succession law with much lower suppression needs; if load-bearing, suppressing theological-political dissent is intrinsic and the measured suppression is irreducible without abandoning the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_sanction_warrant_status, empirical, 'Whether the theological warrant or the statutory succession rule carries the arrangement''s enforcement burden.').

omega_variable(
    succession_coordination_robustness,
    'Does the succession-coordination function survive ambiguous succession, or does it fail exactly when it is most needed?',
    'Comparative history of contested successions (cousins'' wars, multi-claimant thrones, regencies): measure armed-conflict incidence against the clarity of the applicable succession rule.',
    'If coordination collapses under ambiguity, the genuine-coordination half of the ledger shrinks and the arrangement trends toward pure extraction maintained by force alone; if robust, the coordination credit is real and durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_coordination_robustness, empirical, 'Robustness of the orderly-succession function under stress cases.').

omega_variable(
    subject_coalition_capacity,
    'Subjects are individually powerless; does episodic coalition (revolt, revolution) convert the payer seat into an organized actor whose resistance reshapes the arrangement?',
    'Code major revolts and revolutions across the interval for whether they altered succession law, fiscal terms, or participation rights, versus being suppressed without structural change.',
    'Sustained coalition capacity raises the effective resistance of the payer seat and accelerates drift toward the hybrid reading; repeated coalition failure entrenches the current profile and lengthens the interval''s tail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_coalition_capacity, empirical, 'Whether diffuse payer resistance aggregates into structurally effective opposition.').

omega_variable(
    cs_framing_underdetermination,
    'Is the declared framing — a formalized succession-law kernel under lineage authority — the only defensible one, or does an alternative framing (the anointing ritual as a practice-based kernel, or heraldic and courtly adjudication as an implicit kernel) fit the same institution?',
    'Examine borderline succession disputes: whichever mechanism actually decides them — statute text, ritual performance, or adjudicated genealogy — is the operative kernel. If ritual decides, the kernel is practice-based; if open-ended courtly judgment decides, it is implicit.',
    'A practice-based framing would reclassify authority_grounding from lineage to practice and alter the computed drift profile; the foreclosure relations to the sibling readings would remain but their strength would soften, since a practice-kernel absorbs revision more readily than a statute-kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the commitment-system kernel beneath the monarchical reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_leg_monarch_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sov_leg_monarch_tr_t0, observed).
narrative_ontology:measurement(sov_leg_monarch_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(sov_leg_monarch_tr_t40, observed).
narrative_ontology:measurement(sov_leg_monarch_tr_t80, sovereign_legitimacy__monarchical_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement_basis(sov_leg_monarch_tr_t80, observed).
narrative_ontology:measurement(sov_leg_monarch_tr_t120, sovereign_legitimacy__monarchical_reading, theater_ratio, 120, 0.34).
narrative_ontology:measurement_basis(sov_leg_monarch_tr_t120, observed).
narrative_ontology:measurement(sov_leg_monarch_tr_t160, sovereign_legitimacy__monarchical_reading, theater_ratio, 160, 0.37).
narrative_ontology:measurement_basis(sov_leg_monarch_tr_t160, observed).
narrative_ontology:measurement(sov_leg_monarch_tr_t200, sovereign_legitimacy__monarchical_reading, theater_ratio, 200, 0.4).
narrative_ontology:measurement_basis(sov_leg_monarch_tr_t200, observed).
narrative_ontology:measurement(sov_leg_monarch_tr_t240, sovereign_legitimacy__monarchical_reading, theater_ratio, 240, 0.42).
narrative_ontology:measurement_basis(sov_leg_monarch_tr_t240, observed).

% Extraction over time
narrative_ontology:measurement(sov_leg_monarch_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(sov_leg_monarch_be_t0, observed).
narrative_ontology:measurement(sov_leg_monarch_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(sov_leg_monarch_be_t40, observed).
narrative_ontology:measurement(sov_leg_monarch_be_t80, sovereign_legitimacy__monarchical_reading, base_extractiveness, 80, 0.61).
narrative_ontology:measurement_basis(sov_leg_monarch_be_t80, observed).
narrative_ontology:measurement(sov_leg_monarch_be_t120, sovereign_legitimacy__monarchical_reading, base_extractiveness, 120, 0.65).
narrative_ontology:measurement_basis(sov_leg_monarch_be_t120, observed).
narrative_ontology:measurement(sov_leg_monarch_be_t160, sovereign_legitimacy__monarchical_reading, base_extractiveness, 160, 0.68).
narrative_ontology:measurement_basis(sov_leg_monarch_be_t160, observed).
narrative_ontology:measurement(sov_leg_monarch_be_t200, sovereign_legitimacy__monarchical_reading, base_extractiveness, 200, 0.71).
narrative_ontology:measurement_basis(sov_leg_monarch_be_t200, observed).
narrative_ontology:measurement(sov_leg_monarch_be_t240, sovereign_legitimacy__monarchical_reading, base_extractiveness, 240, 0.72).
narrative_ontology:measurement_basis(sov_leg_monarch_be_t240, observed).

% Suppression requirement over time
narrative_ontology:measurement(sov_leg_monarch_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(sov_leg_monarch_su_t0, observed).
narrative_ontology:measurement(sov_leg_monarch_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(sov_leg_monarch_su_t40, observed).
narrative_ontology:measurement(sov_leg_monarch_su_t80, sovereign_legitimacy__monarchical_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement_basis(sov_leg_monarch_su_t80, observed).
narrative_ontology:measurement(sov_leg_monarch_su_t120, sovereign_legitimacy__monarchical_reading, suppression_requirement, 120, 0.69).
narrative_ontology:measurement_basis(sov_leg_monarch_su_t120, observed).
narrative_ontology:measurement(sov_leg_monarch_su_t160, sovereign_legitimacy__monarchical_reading, suppression_requirement, 160, 0.73).
narrative_ontology:measurement_basis(sov_leg_monarch_su_t160, observed).
narrative_ontology:measurement(sov_leg_monarch_su_t200, sovereign_legitimacy__monarchical_reading, suppression_requirement, 200, 0.76).
narrative_ontology:measurement_basis(sov_leg_monarch_su_t200, observed).
narrative_ontology:measurement(sov_leg_monarch_su_t240, sovereign_legitimacy__monarchical_reading, suppression_requirement, 240, 0.78).
narrative_ontology:measurement_basis(sov_leg_monarch_su_t240, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'legitimate authority' (or 'sovereignty') covers three structurally distinct claims. This file authors the downward divine-bloodline claim (epsilon 0.72, victims = excluded subjects, validation by bloodline and ritual). The republican_reading authors the upward popular-consent claim (different victim set — unelected authority holders — and different validation mechanism). The constitutional_hybrid_reading authors the dual-sourced claim (smallest victim set, lowest suppression, constitutional mediation absorbing drift). The monarchical reading is upstream historically: its crises created the conditions under which the hybrid reading became holdable, and its persistence supplies the surviving crowns the hybrid allocates. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__monarchical_reading, institutional, 0.22).
constraint_indexing:directionality_override(sovereign_legitimacy__monarchical_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
