% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Classification-Scaled Geneva Protections under Commander-Assessed Proportionality (Hybrid Reading)
 *   domain: legal/international_humanitarian_law/armed_conflict_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Geneva protective-scope kernel
 *   — the hybrid_proportionality_reading, under which treaty protections
 *   legitimately scale with the legally determined conflict type (Additional
 *   Protocol I package for international armed conflict; Common Article 3 and
 *   Additional Protocol II floors for non-international conflict) and
 *   proportionality analysis determines application in particular
 *   engagements. The reading's structural signature: the victim set varies
 *   with conflict classification, and the classification call plus the
 *   proportionality determination are made predominantly by the militarily
 *   stronger party through its own legal machinery. Genuine coordination and
 *   asymmetric advantage coexist in one structure — hence the tangled-rope
 *   claim, authored independently of the metrics. The colloquial label
 *   'Geneva protections' decomposes (epsilon-invariance) into three
 *   structurally distinct readings of the same kernel: this one, a
 *   state-centric reading gating treaty scope on Article 4 combatant
 *   criteria, and a universal-rights reading installing a status-blind floor;
 *   each is a separate story linked through network.affects_constraints. KEY
 *   AGENTS (by structural relationship): - militarily_dominant_states:
 *   primary beneficiary (institutional/arbitrage) — controls which tier
 *   governs its own operations and captures the flexibility -
 *   military_legal_advisory_corps: secondary beneficiary
 *   (organized/identity_locked) — staffs and reproduces the
 *   classification-proportionality apparatus -
 *   civilians_in_niac_classified_theaters: primary target (powerless/trapped)
 *   — receives whichever floor the opposing party selected -
 *   irregular_fighters_without_combatant_status: primary target
 *   (moderate/trapped) — detained and prosecuted under the detainer's chosen
 *   regime - icrc_custodial_machinery: administrator
 *   (institutional/constrained) — custodial enforcement without coercive
 *   sanction - international_criminal_tribunals: analytical observer
 *   (institutional/analytical) — fixes or contests classifications ex post -
 *   non_state_armed_groups: excluded seat (moderate/trapped) — bound by the
 *   tiers, never consulted in drafting them
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Classification-Scaled Geneva Protections under Commander-Assessed Proportionality (Hybrid Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/international_humanitarian_law/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '51d7d889-44c5-4d7e-9dc7-593ab9477f29').
narrative_ontology:cs_kernel_codification('51d7d889-44c5-4d7e-9dc7-593ab9477f29', fixed_text).
narrative_ontology:cs_authority_grounding('51d7d889-44c5-4d7e-9dc7-593ab9477f29', distributed).
narrative_ontology:cs_reading_relation('51d7d889-44c5-4d7e-9dc7-593ab9477f29', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('51d7d889-44c5-4d7e-9dc7-593ab9477f29', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('51d7d889-44c5-4d7e-9dc7-593ab9477f29', foundational, protection_legitimately_scales_with_conflict_classification).
narrative_ontology:cs_axiom_status(protection_legitimately_scales_with_conflict_classification, holdable).
narrative_ontology:cs_axiom_grounding('51d7d889-44c5-4d7e-9dc7-593ab9477f29', protection_legitimately_scales_with_conflict_classification, conventional).
narrative_ontology:cs_axiom('51d7d889-44c5-4d7e-9dc7-593ab9477f29', foundational, commander_assessed_proportionality_disciplines_force_application).
narrative_ontology:cs_axiom_status(commander_assessed_proportionality_disciplines_force_application, holdable).
narrative_ontology:cs_axiom_grounding('51d7d889-44c5-4d7e-9dc7-593ab9477f29', commander_assessed_proportionality_disciplines_force_application, instrumental).
narrative_ontology:cs_reference_frame('51d7d889-44c5-4d7e-9dc7-593ab9477f29', classification_scaled_proportionate_application).
narrative_ontology:cs_drift_state('51d7d889-44c5-4d7e-9dc7-593ab9477f29', contemporary_asymmetric_conflicts, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('51d7d889-44c5-4d7e-9dc7-593ab9477f29', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, militarily_dominant_states).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_legal_advisory_corps).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_niac_classified_theaters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, irregular_fighters_without_combatant_status).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, conflict_classification_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, commander_led_proportionality_assessment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Field large expeditionary forces across borders without declaring war, then decide for themselves which legal tier governs each operation: labeling an intervention non-international keeps it under the lighter Common Article 3 floor and avoids Additional Protocol I obligations, while labeling it international unlocks privileges their adversaries rarely enjoy. Proportionality reviews run inside their own legal-adviser chains before strikes. They can revisit classifications as operations evolve, decline to join protocols they dislike, and set precedent through practice at a scale no challenger matches.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, militarily_dominant_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, militarily_dominant_states, agenda_setter).

% Staff the classification memoranda, collateral-damage estimation methods, and targeting-law reviews through which the tiered system operates day to day. Careers, promotion boards, and doctrinal authority inside defense establishments are built on this apparatus; dissenting from the framework means stepping outside the profession rather than relocating within it.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_legal_advisory_corps, beneficiary,
    organized, biographical, identity_locked, global).

% Live in the districts where a stronger power operates under the lighter tier. Their entitlement to warning, precaution, and humane treatment runs through the Common Article 3 and Additional Protocol II floor rather than the fuller Additional Protocol I package, and which floor applies was decided by the party operating against them. Leaving the theater is often impossible, and contesting the classification is not a channel open to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_niac_classified_theaters, payer,
    powerless, immediate, trapped, regional).

% Take up arms in conflicts their opponents label non-international. Captured, they are registered as security detainees rather than prisoners of war: no combatant privilege, no repatriation-on-cessation entitlement, prosecution under the detaining state's domestic law. They cannot accede to the instruments that fix their treatment and have no standing from which to challenge the classification that determined it.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, irregular_fighters_without_combatant_status, payer,
    moderate, biographical, trapped, regional).

% Addressed by Additional Protocol II and bound by Common Article 3, but they had no seat in the diplomatic conferences that drafted the tiers, cannot become party to the instruments, and hold no forum in which to argue that their conflicts merit the heavier tier or that an opponent's proportionality determinations failed. Their objections surface only through intermediaries or after the fact.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, excluded,
    moderate, generational, trapped, regional).

% Hold the treaties' custodial role: disseminate the rules, register and visit detainees under confidential dialogue, press parties on the consequences of their classification choices, and broker the access agreements that give the lighter tier substance. Their leverage is persuasion and presence, not sanction; where a detaining power denies access under a downgraded classification, the machinery absorbs the failure without a corrective channel.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, icrc_custodial_machinery, agenda_setter,
    institutional, generational, constrained, global).

% Adjudicate alleged violations after the fact. Their casework fixes or contests conflict classifications and proportionality judgments, building the precedent layer states later cite at one another. They act only where states refer cases or permit investigation, so their reach tracks the cooperation of the same powerful parties whose choices they would scrutinize.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, militarily_dominant_states).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives belligerents a ratified, operable legal framework for conducting hostilities: the two-tier design (full Additional Protocol I standards for international armed conflict, Common Article 3 and Additional Protocol II floors for internal conflict) was the compromise that achieved near-universal ratification, gives commanders determinate rules, and guarantees a minimal humane-treatment floor in civil wars where previously no treaty law applied.
% TRANSFER_FUNCTION: Moves obligation-weight and risk unevenly across parties: classification discretion lets stronger actors shed the heavier tier's obligations for their own operations, shifting compliance burden, legal certainty, and detention/treatment risk onto weaker parties, captured fighters, and civilian populations — while certainty of protection concentrates on categories (prisoners of war in declared interstate wars) the strong are likeliest to encounter symmetrically.
% ABSENT_VOICES: Non-state armed groups and the civilian populations of downgraded-theater conflicts would object if present — they would argue for a single floor regardless of classification and against attacker-side proportionality self-assessment — but they hold no drafting seat, cannot accede to the instruments, and are represented only indirectly by ICRC advocacy and humanitarian NGOs.
% DISAPPEARANCE_RATIONALE: If the classification-scaled regime vanished overnight, military legal-adviser chains would lose the framework that structures every targeting memorandum and detention decision, ICRC access and registration agreements would collapse with it, and the Common Article 3 floor — the only treaty protection operating in most contemporary conflicts, which are non-international — would disappear from civil wars entirely. Tribunal casework, doctrine, and training pipelines would all require rebuilding around some successor framework.
% FOUNDING_PROBLEM: Mid-twentieth-century treaty law protected victims of interstate war but left civil wars legally bare, and a single maximal standard for all conflict types was diplomatically unratifiable; the tiered design was built to solve ratification while extending at least a minimum floor to internal conflict.
% FOUNDING_PROBLEM_CORROBORATION: The founding gap is attested outside the beneficiary set: 1949 and 1974-77 Diplomatic Conference records document the civil-war protection hole the tiering addressed; ICRC custodial commentaries (including the updated Common Article 3 commentary) trace the problem's persistence into contemporary non-international conflicts; independent academic IHL scholarship corroborates both that the original problem remains live and that its current solution's adequacy is disputed by the universal-floor camp. The populations actually governed by the lighter tier attest nothing directly — they have no seat, which is itself signal.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.62 because the arrangement's genuine floor protections (Common Article 3 saves measurable lives in internal conflicts; Additional Protocol I protections are real where the tier applies) are netted against classification discretion that thins protection exactly where the strongest parties operate, and proportionality determinations that are self-assessed by the attacking side. Suppression is 0.58 as a RAW structural property — unscaled by power or scope per the framework's rule: the arrangement persists partly through coercive enforcement machinery (courts-martial, ICRC access leverage, tribunal referral) and partly through blocked alternatives (great-power refusal to ratify Additional Protocol I, reservation practices, resistance to the human-rights-law overlay that would restore withdrawn protections). Theater ratio 0.42: the custodial visit function and the floor's lifesaving operation are real, but a growing share of the apparatus — classification memoranda written to survive litigation, proportionality reviews functioning as liability shields — performs legality rather than producing restraint. Accessibility collapse is 0.48, deliberately moderate: once the tiering is understood, alternatives do not vanish — the universal-floor reading and the human-rights-law overlay remain live, partially accessible channels, which is precisely why the constraint needs continuous enforcement. Resistance 0.57 reflects sustained contestation: NGO classification challenges, UN commission findings disputing proportionality verdicts, state-to-state accusation cycles, and tribunal litigation. The temporal series run on ONE shared grid (1949, 1969, 1977, 1991, 2001, 2011, 2025) with all three tracked metrics authored at every point. Base extractiveness ratchets upward with each expansion of classification discretion — the 1977 protocol tiering formalizes the two-track structure, the post-Cold War intervention era normalizes self-classified expeditions, and the 2001-era transnational-conflict classifications push it to peak — with slight relief by 2025 as human-rights-law jurisprudence regains ground. Theater climbs with the legal-cover apparatus. Suppression requirement rises as enforcement infrastructure matures, then plateaus once the machinery is built out.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structural data. From the militarily dominant state's seat the arrangement is a ratifiable, operable legal order it helped build — coordination it administers, with flexibility it regards as necessary prudence. From the trapped civilian's and the detained fighter's seats the same structure operates as protection contingent on the adversary's paperwork: their tier was selected by the party operating against them, and no channel exists to contest the selection. The legal-advisery corps experiences identity fusion — the framework is their profession — so exit is unthinkable even where doubt exists. The ICRC custodian sees enforcement without sanction: it administers compliance it cannot compel and absorbs each access denial. Tribunal observers see contested precedent accumulating. The engine computes these per-seat classifications from the structural atoms; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Militarily dominant states hold beneficiary declarations with arbitrage-grade exit (reclassify, reserve, abstain from protocols), placing them near the beneficiary end — effective extraction dampens toward subsidy, since the arrangement shields them from obligations. The legal-advisery corps is a derived-status beneficiary with identity_locked exit, sitting beneficiary-adjacent but unable to arbitrage. Civilians in downgraded theaters and captured irregular fighters carry victim declarations with trapped exit, placing them near the full-target end — effective extraction amplifies for them. The ICRC machinery declares neither benefit nor victimhood: as administrator its position is approximately symmetric, absorbing costs without collecting the flexibility rents. Tribunals hold analytical seats. No directionality_overrides were needed: the beneficiary/victim declarations plus exit atoms reproduce every seat's true structural relationship, and the override mechanism keys on power atoms too coarsely to improve on the derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents both symmetrical mislabels. Reading the arrangement as pure rope (a humanitarian achievement, full stop) erases who pays for the flexibility: the victim set is real and classification-correlated. Reading it as pure snare (an evasion license dressed as law) erases the measured lives saved by the Common Article 3 floor and the genuine ratification bargain the tiering enabled. The tangled-rope classification forces both halves into view: coordination function (near-universal ratification of an operable framework; a floor where previously nothing applied) AND asymmetric extraction (obligation-shedding concentrated in the strongest hands) held together by active enforcement. Mandatrophy is NOT resolved: the founding problem — persons unprotected in conflicts lacking clean interstate form — is live, the arrangement remains load-bearing, and no sunset clause ever existed (treaties of indefinite duration), so scaffold and piton checks fail on their structural gates. The R5 mismatch consumer finds status=live x verdict=world_rearranges: no zombie flag; the arrangement is doing work, some of it extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_relative_structural_delta,
    'This constraint is one reading of kernel geneva_conventions_protective_scope — how would epsilon and the victim set restructure under the sibling readings?',
    'Instantiate the sibling stories separately and compare computed types and per-seat chi: the universal_rights_reading raises the floor status-blind (shrinking the classification-varying victim set and lowering epsilon toward coordination cost); the state_centric_reading narrows coverage to Article 4 combatants (removing the floor for irregular fighters entirely and relocating extraction onto the excluded).',
    'The victim set and effective extraction are properties of the reading, not the topic; cross-reading comparisons of this story''s numbers against siblings measure the kernel''s contest, not measurement error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relative_structural_delta, conceptual, 'Committer-frame routing: structural delta of this reading versus its siblings.').

omega_variable(
    classification_boundary_indeterminacy,
    'Is the international/non-international boundary determinable for transnational, hybrid, and intervention-without-declaration conflicts, or does indeterminacy systematically default discretion to the strongest interpreter?',
    'Comparative coding of state classification decisions against battlefield incentives across post-1990 conflicts, combined with jurisprudential analysis of tribunal holdings on the boundary.',
    'If the boundary is systematically indeterminate where strong powers operate, classification-scaling collapses into discretionary self-grading, raising effective extraction above the authored 0.62 and pushing the seat computation toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_boundary_indeterminacy, empirical, 'Whether the tiering''s input variable is legally determinable or power-contingent.').

omega_variable(
    proportionality_determination_auditability,
    'Do commander-led proportionality determinations actually discipline force application, or do they function as ex ante liability laundering?',
    'Audit studies comparing contemporaneous legal reviews with subsequent casualty recordings, weapons-effects data, and tribunal findings on the same engagements.',
    'Low auditability inflates the theater component of effective extraction (performative legal activity substituting for restraint) and supports remedies requiring external assessment; high concordance would support the instrumental grounding of the reading''s second axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_determination_auditability, empirical, 'Reliability of self-assessed proportionality as a restraint mechanism.').

omega_variable(
    ihrl_overlay_restoration,
    'Does the human-rights-law overlay actually restore protections that classification-scaling withdraws in downgraded conflicts, or does state resistance render it aspirational in practice?',
    'Track detention-regime and targeting-practice outcomes in classified-NIAC theaters where the overlay is invoked (court challenges, treaty-body findings, commission-of-inquiry recommendations) versus where it is not.',
    'Effective restoration lowers realized suppression and partially closes the victim set''s exit gap; ineffective restoration leaves the trapped seats with no working alternative channel and confirms the accessibility-collapse estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ihrl_overlay_restoration, empirical, 'Whether the principal alternative channel to classification-scaled protection functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(gene_tr_t1969, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1969, 0.18).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(gene_tr_t1991, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(gene_tr_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2011, 0.44).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1949, 0.34).
narrative_ontology:measurement(gene_be_t1969, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1969, 0.39).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.46).
narrative_ontology:measurement(gene_be_t1991, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1991, 0.53).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2001, 0.61).
narrative_ontology:measurement(gene_be_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2011, 0.64).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(gene_su_t1969, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1969, 0.44).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(gene_su_t1991, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1991, 0.54).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2001, 0.56).
narrative_ontology:measurement(gene_su_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the 'Geneva protections' label per the epsilon-invariance principle. The colloquial concept conflates three structurally distinct claims about the same fixed-text kernel: this hybrid reading (coverage and protection level both scale with legally determined conflict type; epsilon 0.62, victim set classification-correlated), the state_centric_reading (coverage gated on Article 4 criteria; epsilon concentrated on excluded unprivileged belligerents), and the universal_rights_reading (status-blind floor; epsilon compressed toward coordination cost). This story links both siblings because the hybrid reading is the pivot: it inherits the state-centric reading's tier architecture while conceding the universal reading's floor, and each sibling's proponents cite the hybrid's ambiguities as evidence for their own position. Each member carries a stable, single-referent epsilon; the contest lives in the family, not in any one file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
