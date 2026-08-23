% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: Mandatory Interpretive Discretion Without Binding Review (Balfour Mandate Kernel)
 *   domain: international law/colonial administration/state formation
 *
 * SUMMARY:
 *   The mandate instruments contained two obligations that pointed in
 *   different directions - facilitation of a Jewish national home and
 *   protection of the existing population's civil and religious rights - and
 *   assigned to one power the job of making them governable together. This
 *   story isolates the discretion component: the mandatory's authority to
 *   adjudicate between competing readings of its own instrument, without any
 *   binding external review. Interpretation was never revised at the level of
 *   the text; it was re-issued as policy - Churchill 1922, Passfield 1930,
 *   the MacDonald letter 1931, Peel 1937, the White Paper 1939, the Land
 *   Transfers Regulations 1940 - each issuance resetting the baseline on
 *   which both communities had capitalized. The discretion itself, not any
 *   particular content it produced, is the operational constraint: whoever
 *   held unreviewable adjudication held the territory's strategic tempo. KEY
 *   AGENTS (by structural relationship): - british_colonial_administrators:
 *   Agenda-setting beneficiary (institutional/arbitrage) - holds and
 *   exercises adjudicative discretion - imperial_strategy_planners: Secondary
 *   beneficiary (institutional/global arbitrage) - collects strategic
 *   flexibility without bearing local costs - palestinian_arab_communities:
 *   Primary target (organized/trapped) - bears the oscillation tax on land,
 *   representation, and strategy - zionist_institutions_yishuv: Primary
 *   target (organized/constrained) - bears repricing risk on built
 *   institutions; partial non-binding recourse -
 *   tenant_farmers_on_conveyed_lands: Diffuse local target
 *   (powerless/trapped) - absorbs transfer-regime revisions retroactively -
 *   permanent_mandates_commission: Analytical observer - the neutered
 *   external reviewer whose advice could not bind -
 *   exiled_arab_leadership_after_1937: Excluded voice - removed from
 *   conversation by the enforcement the discretion required
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.8).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "Mandatory Interpretive Discretion Without Binding Review (Balfour Mandate Kernel)").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international law/colonial administration/state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, 'a164672e-0cc5-48ae-aa77-2c35409c4c52').
narrative_ontology:cs_kernel_codification('a164672e-0cc5-48ae-aa77-2c35409c4c52', fixed_text).
narrative_ontology:cs_authority_grounding('a164672e-0cc5-48ae-aa77-2c35409c4c52', lineage).
narrative_ontology:cs_interpretation_layer_present('a164672e-0cc5-48ae-aa77-2c35409c4c52').
narrative_ontology:cs_reading_relation('a164672e-0cc5-48ae-aa77-2c35409c4c52', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('a164672e-0cc5-48ae-aa77-2c35409c4c52', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('a164672e-0cc5-48ae-aa77-2c35409c4c52', foundational, mandatory_final_interpretive_authority).
narrative_ontology:cs_axiom_status(mandatory_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('a164672e-0cc5-48ae-aa77-2c35409c4c52', mandatory_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('a164672e-0cc5-48ae-aa77-2c35409c4c52', secondary, discretionary_balance_within_instrument_bounds).
narrative_ontology:cs_axiom_status(discretionary_balance_within_instrument_bounds, holdable).
narrative_ontology:cs_axiom_grounding('a164672e-0cc5-48ae-aa77-2c35409c4c52', discretionary_balance_within_instrument_bounds, instrumental).
narrative_ontology:cs_reference_frame('a164672e-0cc5-48ae-aa77-2c35409c4c52', league_trusteeship_accountability).
narrative_ontology:cs_drift_state('a164672e-0cc5-48ae-aa77-2c35409c4c52', late_mandate_insurgency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a164672e-0cc5-48ae-aa77-2c35409c4c52', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, imperial_strategy_planners).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_institutions_yishuv).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, tenant_farmers_on_conveyed_lands).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, fiduciary_trusteeship_doctrine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, administrative_judgment_supremacy_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The High Commissioner's secretariat and the Colonial Office Palestine department ran the courts, land registries, and immigration quota machinery, and issued the periodic White Papers that re-set policy baselines. Their reading of the mandate text was final: no body in the system could overrule an interpretation they announced, and they declined referral of disputed decrees to external courts. Officers rotated through on fixed tours and moved onward to other postings, so no individual administrator was stuck living inside any policy they had authored.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, beneficiary).

% Foreign Office, Admiralty, and Air Staff planners valued Palestine for its position astride the Suez approaches, the Kirkuk-Haifa oil pipeline terminus, and the Mediterranean air route. Interpretive flexibility let London re-weight its promises as strategic requirements shifted - wartime basing, pipeline security, regional diplomacy - without formally breaking any treaty text. They did not administer daily life in Palestine and rarely bore the local costs of the reversals their flexibility produced.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, imperial_strategy_planners, beneficiary,
    institutional, generational, arbitrage, global).

% Muslim-Christian Associations, the Arab Executive, and later the Arab Higher Committee petitioned London and Geneva, organized boycotts, and finally revolted in 1936-39. Land tenure, political representation, and exposure to immigration all turned on readings announced in Jerusalem and London; each White Paper repriced property, institutions, and political strategy at a stroke. Leaving meant abandoning land and kin networks, so exit was not a live option. Episodic favors - the Passfield paper's restraint on land sales, the 1939 restrictions on Jewish purchase - arrived unbidden and were revoked or hollowed without notice.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_communities, payer,
    organized, generational, trapped, national).

% The Zionist Executive and, after 1929, the Jewish Agency built schools, labor federations, land-purchase consortia, and settlement bodies around promised facilitation of the national home. Each policy reversal - 1930's land controls, 1939's immigration ceiling of 75,000 and transfer zoning - repriced decades of accumulated investment overnight. Unlike their Arab counterparts they held League-recognized consultative status and diaspora diplomatic channels, giving partial recourse that proved ultimately non-binding: petitions reached Geneva but returned as advice the mandatory was free to disregard. Ideological fusion with the national-home project made exit unthinkable regardless of cost.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_institutions_yishuv, payer,
    organized, generational, constrained, global).

% Fellahin cultivating land sold through the conveyance machinery encountered each new transfer ordinance retroactively. Compensation formulas for displaced cultivators were rewritten in 1929, 1933, and 1940, each revision altering what a completed dispossession was worth. Petitions travelled upward to the same offices that drafted the rules; no external court entertained their claims, and few could read the instruments that governed their tenure.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, tenant_farmers_on_conveyed_lands, payer,
    powerless, immediate, trapped, local).

% The Geneva body of jurists and former colonial administrators examined annual reports, heard petitions through a dedicated bureau, and issued conclusions and recommendations on disputed questions - land policy, immigration, the Western Wall. Its advice was advisory by construction, Britain declined to submit contested decrees to the Permanent Court of International Justice, and the commission's leverage stopped where British assent stopped.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% After the suppression of the revolt the Higher Committee's leaders were detained or fled the country. Thereafter Arab positions entered negotiation only when London summoned them, as at the 1939 St James Conference, and the standing petition-and-audit channels ran through intermediaries the mandatory found acceptable. The people most directly harmed by the interpretive regime lost their seat in the conversation about it.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, exiled_arab_leadership_after_1937, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single authoritative interpreter resolves collisions between the mandate's dual obligations - facilitating the Jewish national home and safeguarding the civil and religious rights of the existing population - so that courts, land registries, and immigration administration each run on one working meaning at a time.
% TRANSFER_FUNCTION: Moves interpretive certainty itself: grants and revokes policy baselines (immigration ceilings, land-transfer rules, constitutional assurances) to whichever community's cooperation is momentarily useful, collecting flexibility, compliance, and strategic option value from both, and moving strategic dependence from both communities toward the administering power.
% ABSENT_VOICES: The missing seat is a binding external reviewer - a League organ or court empowered to overrule mandatory interpretation. Britain withdrew the Nationality Decrees in 1923 rather than submit them to the Permanent Court of International Justice, and kept the Permanent Mandates Commission's findings advisory. After 1937 the exiled Arab leadership was likewise outside the negotiated channels except when London chose to convene it.
% DISAPPEARANCE_RATIONALE: Petition budgets, lobbying networks, land-purchase hedging, and eventually revolt logistics on both sides were capitalized against the next reading arriving from Jerusalem or London. Remove the discretion overnight and both communities immediately renegotiate against fixed text and against each other, whatever arbitration organ the League possessed inherits the function Britain monopolized, and the White-Paper baseline ladder loses its ratchet - the entire strategic posture of the territory reorganizes within months.
% FOUNDING_PROBLEM: Two wartime commitments - the Balfour Declaration and the obligations implied toward the existing population - had to be administered by a single power under a League instrument containing both obligations without ranking them, in a territory whose courts and land system needed one operative meaning to function at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the benefiting parties: Permanent Mandates Commission sessional records repeatedly noted that disputed questions were resolved solely by mandatory fiat and recommended firmer textual anchoring; the 1923 Nationality Decrees episode demonstrates a competent external arbiter existed and its jurisdiction was declined; and the petition archives of both communities attest that they sought precisely the fixed-review alternative. The administering power alone attests that unreviewable discretion was the necessary form of the solution.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (interval end, matching the final measurement) because the uncertainty tax was levied on both communities simultaneously and decoupled from any service only the discretionary arrangement could render: adjudication itself could have been supplied by a reviewable arbiter, so the premium charged above ordinary adjudication is the extractive margin. Suppression is authored at 0.80 and is a raw structural property, deliberately NOT scaled by power or scope in this field - the engine owns that arithmetic; the raw figure reflects the enforcement arc from petition management in the 1920s through the Defence (Emergency) Regulations and wartime censorship of the 1940s, which is why the suppression_requirement series is tracked on the shared grid. Theater_ratio ends at 0.48: the commissions of inquiry (Hope Simpson, Peel, Woodhead) increasingly functioned as deferral devices - convene, take evidence, shelve - while governance continued on the mandatory's unilateral reading. The measurement series runs on one shared time grid (ten points, 1920-1947 mapped to t=0..27) with every tracked metric authored at every point. Cyclical dynamics are documented rather than smoothed: resets cluster at 1922, 1930-31, 1937, and 1939-40, and the oscillation is itself partly the extraction mechanism - each cycle of grant-pressure-revocation taught both communities that only current favor mattered, rewarding defection between them and structurally preventing the joint victim coalition that a pure snare's targets would otherwise attempt.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the administering seat the same structure appears as fiduciary craft - judgment exercised under an ambiguous trust, exactly what the instrument demanded. From either community seat it appears as a rigged casino in which every accumulated asset is repriced at the next issuance. The permanent_mandates_commission seat, analytical and outside, sees an unreviewable judge wearing a reviewer's costume. Among same-level actors, the two community seats differ on exit despite comparable organized power: the yishuv's diaspora networks and League consultative status gave constrained recourse, while the Arab communities' territorial entanglement gave none - and the yishuv's ideological fusion with the national-home project (identity-lock: exit unthinkable regardless of price) contrasts with the Arab communities' relational-place lock (identity constituted through land and locality). If either identity frame broke - if the yishuv had treated the mandate as merely one host polity among options, or Arab elites had treated land loss as recoverable through emigration - the effective extraction on that seat would fall sharply even with identical structural barriers.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. british_colonial_administrators sit near the beneficiary pole: discretion subsidizes them with policy autonomy and career insulation, and arbitrage-grade exit (rotation, redeployment) pushes them further from the target end. imperial_strategy_planners collect the same flexibility at global scope while bearing none of the local cost. The three victim groups sit near the target end, scaled by exit quality: tenant farmers (trapped, local, immediate horizon) approach the full-target pole; palestinian_arab_communities (trapped, generational) close behind; zionist_institutions_yishuv derive d below raw victim derivation because episodic discretionary grants - early immigration facilitation, agency recognition, the 1931 letter - intermittently subsidized them even as the revocability of every grant taxed everything they built. That episodic-benefit structure is why a directionality override is declared at the organized power atom (d=0.72): both community seats are net targets whose raw victim-derived directionality would overshoot, and the override applies cleanly because both share the atom and the same episodic-benefit complication. No override is declared at the institutional atom, where the seats genuinely diverge (administrators and planners near the beneficiary pole, the commission analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here guards against laundering in both directions. Claiming snare blocks the rope-reading - 'someone must adjudicate, therefore this adjudicator is coordination' - by insisting the adjudication demand is met by the broader administrative apparatus while the discretion-WITHOUT-REVIEW component specifically is what blocks every exit toward fixed meaning: declining PCIJ jurisdiction, keeping commission advice advisory, re-issuing rather than revising. Conversely, the analysis refuses a pure-predation mislabel: the founding adjudication problem was real, remains live as a problem, and the arrangement did deliver one operative meaning at a time; what it never delivered was a way for anyone but the mandatory to know what next week's meaning would be. On the genealogy interview the founding_problem_status is contested rather than dead - the collision the arrangement was built to manage persisted throughout - but the corroboration record shows the specific solution (unreviewable discretion) was contested from outside the beneficiary set from 1923 onward, which is the mismatch signature the consumer checks against capture rather than obsolescence. No piton decay is asserted: the discretion's function was extractive-operational from the start and did not atrophy into performance; theater rose but functional unilateral adjudication never ceased.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_provenance,
    'How does instantiating the discretion reading change the victim set and epsilon relative to the sibling readings of the same kernel?',
    'Compile all three family stories and compare computed per-seat types and effective extraction; the disagreement localizes exactly where each reading fixes textual meaning differently.',
    'Under the primacy reading the victim set collapses to the Arab population and the yishuv becomes a beneficiary; under the indigenous-rights reading the polarity inverts; under this reading BOTH communities are targets and the administering apparatus is the sole beneficiary. Cross-reading differences measure the kernel''s contest structure and are signal, not error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer-frame routing: this story is one reading of the balfour_mandate_instruments kernel, and its structural deltas are meaningful only against the sibling readings.').

omega_variable(
    binding_external_review_counterfactual,
    'Would a binding external reviewer have produced materially different policy paths than the advisory regime?',
    'Counterfactual analysis of episodes where advisory findings collided with British practice: the 1923 Nationality Decrees withdrawal rather than PCIJ submission, Permanent Mandates Commission criticisms of land policy 1930-35, the 1937 partition deliberations.',
    'If Britain would likely have complied under binding review, the discretion component''s marginal extraction over ordinary adjudication is confirmed large and the snare reading strengthens; if defiance was near-certain, discretion''s distinctness from any conceivable mandate shrinks and epsilon falls toward the administrative floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_external_review_counterfactual, empirical, 'Whether the absence of binding review, rather than the mandate''s existence as such, is the load-bearing extractive feature.').

omega_variable(
    tutelage_sunset_clause_status,
    'Does the Covenant Article 22 tutelage framing constitute a sunset clause binding interpretive discretion to eventual surrender to fixed meaning or local adjudication, or was discretion open-ended within an indefinitely extendable transition?',
    'Legal-historical search for any mandatory text, League Council decision, or commission conclusion that scheduled the discretion component to terminate; none has been located to date, and the mandate ended by war, fiscal exhaustion, and insurgency rather than by built-in transition.',
    'If the tutelage framing binds, the constraint is transitional scaffolding whose persistence past schedule is inertial rather than designed, and the classification migrates toward scaffold-with-violated-sunset; if not, open-ended discretion was the design and the snare reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tutelage_sunset_clause_status, conceptual, 'Whether the mandate''s transitional wrapper ever applied to the discretion component specifically.').

omega_variable(
    oscillation_deliberateness,
    'Was the policy oscillation a deliberate divide-and-rule mechanism or sincerely attempted balancing under irreconcilable obligations?',
    'Colonial Office and Cabinet minute analysis comparing stated deliberative motives against sequencing that consistently maximized imperial option value across the 1922, 1930-31, 1937, and 1939-40 resets.',
    'Deliberate oscillation confirms the intermittent-reinforcement reading of the cyclical measurements - the cycle is the extraction mechanism, not noise - and hardens the snare verdict; sincere balancing under irreconcilable texts would credit residual coordination function to the discretion component itself, pushing toward tangled_rope with lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oscillation_deliberateness, empirical, 'Motivational structure behind the observable policy oscillation; also bears on the blocked victim-coalition question, since a discretionary patron profits from exactly the defection incentives that prevented a joint Arab-Zionist front against the discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(balf_tr_t0, observed).
narrative_ontology:measurement(balf_tr_t2, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 2, 0.28).
narrative_ontology:measurement_basis(balf_tr_t2, observed).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(balf_tr_t5, observed).
narrative_ontology:measurement(balf_tr_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(balf_tr_t8, observed).
narrative_ontology:measurement(balf_tr_t11, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 11, 0.41).
narrative_ontology:measurement_basis(balf_tr_t11, observed).
narrative_ontology:measurement(balf_tr_t14, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 14, 0.43).
narrative_ontology:measurement_basis(balf_tr_t14, observed).
narrative_ontology:measurement(balf_tr_t17, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 17, 0.49).
narrative_ontology:measurement_basis(balf_tr_t17, observed).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(balf_tr_t20, observed).
narrative_ontology:measurement(balf_tr_t23, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 23, 0.44).
narrative_ontology:measurement_basis(balf_tr_t23, observed).
narrative_ontology:measurement(balf_tr_t27, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 27, 0.48).
narrative_ontology:measurement_basis(balf_tr_t27, observed).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(balf_be_t0, observed).
narrative_ontology:measurement(balf_be_t2, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(balf_be_t2, observed).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(balf_be_t5, observed).
narrative_ontology:measurement(balf_be_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(balf_be_t8, observed).
narrative_ontology:measurement(balf_be_t11, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 11, 0.56).
narrative_ontology:measurement_basis(balf_be_t11, observed).
narrative_ontology:measurement(balf_be_t14, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 14, 0.58).
narrative_ontology:measurement_basis(balf_be_t14, observed).
narrative_ontology:measurement(balf_be_t17, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 17, 0.64).
narrative_ontology:measurement_basis(balf_be_t17, observed).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(balf_be_t20, observed).
narrative_ontology:measurement(balf_be_t23, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 23, 0.67).
narrative_ontology:measurement_basis(balf_be_t23, observed).
narrative_ontology:measurement(balf_be_t27, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 27, 0.68).
narrative_ontology:measurement_basis(balf_be_t27, observed).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(balf_su_t0, observed).
narrative_ontology:measurement(balf_su_t2, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 2, 0.37).
narrative_ontology:measurement_basis(balf_su_t2, observed).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(balf_su_t5, observed).
narrative_ontology:measurement(balf_su_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(balf_su_t8, observed).
narrative_ontology:measurement(balf_su_t11, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 11, 0.47).
narrative_ontology:measurement_basis(balf_su_t11, observed).
narrative_ontology:measurement(balf_su_t14, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 14, 0.53).
narrative_ontology:measurement_basis(balf_su_t14, observed).
narrative_ontology:measurement(balf_su_t17, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 17, 0.66).
narrative_ontology:measurement_basis(balf_su_t17, observed).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(balf_su_t20, observed).
narrative_ontology:measurement(balf_su_t23, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 23, 0.76).
narrative_ontology:measurement_basis(balf_su_t23, observed).
narrative_ontology:measurement(balf_su_t27, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 27, 0.8).
narrative_ontology:measurement_basis(balf_su_t27, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% Colloquially 'the mandate' is one thing; per the epsilon-invariance principle it decomposes into a constraint family of three structurally distinct stories sharing the kernel balfour_mandate_instruments. This file isolates the discretion component (WHO adjudicates and WHETHER review binds); the sibling files isolate the substantive readings among which discretion adjudicates. The discretion reading is operationally UPSTREAM of both siblings - it determines which of them is operative at any moment and keeps both permanently available as negotiating currency while preventing either from becoming settled law - and substantively DOWNSTREAM of neither. Epsilon diverges sharply across the family: this story's epsilon prices uncertainty taxation borne by both communities; the primacy reading's epsilon prices asymmetric imposition on the Arab population; the indigenous-rights reading's epsilon prices asymmetric imposition on the Zionist project. Cross-family comparison measures the kernel's contest structure, not measurement inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
