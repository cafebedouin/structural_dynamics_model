% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor-Satisfaction Substrate under Practice Decline (Exogenous Enforcement Reading)
 *   domain: historical sociology / legal anthropology / cultural history
 *
 * SUMMARY:
 *   This file instantiates the practice_decline_reading of the
 *   honor_satisfaction_substrate kernel. The standing arrangement under
 *   analysis is the honor code as a persisting normative substrate — the
 *   inherited grammar of insult, reputation, and satisfaction — whose violent
 *   practice form (dueling) was closed from outside: anti-dueling statutes,
 *   military and professional disciplinary barriers, and a rising opportunity
 *   cost of private satisfaction. On this reading the code did not die and
 *   did not transform into a dignity regime; it survives in attenuated,
 *   functional forms — academy honor codes, regimental tradition,
 *   culture-of-honor regions where insult-response scripts still govern
 *   behavior, and niche continuations wherever enforcement gaps open (student
 *   fencing circles, recorded late duels). The ε referent is therefore this
 *   persisting substrate as this reading sees it: a live normative order with
 *   real member obligations, not a corpse and not the rights-respecting
 *   alternative any reading would endorse. Sibling readings
 *   (cultural_contraction_reading, composite_overdetermined_reading) are
 *   separate constraint files with their own ε, linked through the network
 *   section; they are not folded into this story. The claimed type and the
 *   metrics below are authored independently: the claim states this reading's
 *   structural thesis; the metrics state what the historical record shows
 *   about the substrate's operation across the interval. KEY AGENTS (by
 *   structural relationship): - honor_community_members: Net beneficiary
 *   constituency (organized/identity_locked) — holds the reputation order the
 *   code supplies - status_elite_patriarchs: Beneficiary-administrators
 *   (organized/identity_locked) — adjudicate disputes, collect deference -
 *   young_men_in_honor_cultures: Primary cost-bearers
 *   (moderate/identity_locked) — answer obligations and post-prohibition
 *   legal risk concentrate on them - military_officer_corps: Institutional
 *   beneficiary (institutional/constrained) — honor codes as discipline
 *   infrastructure - state_legal_apparatus: Exogenous enforcer /
 *   agenda_setter (institutional/mobile) — closed the practice envelope by
 *   statute and prosecution - professional_institutions: Barrier builders /
 *   agenda_setter (institutional/mobile) — raised the opportunity cost of
 *   private satisfaction - honor_code_refusers: Excluded dissenters
 *   (powerless/trapped) — bound by the code without its protections -
 *   violence_historians: Analytical observer (analytical/analytical) —
 *   adjudicates the reading contest from archives and field data
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.45).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor-Satisfaction Substrate under Practice Decline (Exogenous Enforcement Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical sociology / legal anthropology / cultural history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a').
narrative_ontology:cs_kernel_codification('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', distributed).
narrative_ontology:cs_authority_grounding('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', practice).
narrative_ontology:cs_interpretation_layer_present('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a').
narrative_ontology:cs_reading_relation('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', foundational, honor_code_persistence_under_suppression).
narrative_ontology:cs_axiom_status(honor_code_persistence_under_suppression, holdable).
narrative_ontology:cs_axiom_grounding('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', honor_code_persistence_under_suppression, empirically_contingent).
narrative_ontology:cs_axiom('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', foundational, exogenous_enforcement_sufficiency).
narrative_ontology:cs_axiom_status(exogenous_enforcement_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', exogenous_enforcement_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', persistent_honor_substrate).
narrative_ontology:cs_drift_state('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', post_prohibition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6ab3dcc2-f790-4e6c-b4c7-f2bd2917677a', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_community_members).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, status_elite_patriarchs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, young_men_in_honor_cultures).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, young_men_in_honor_cultures).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, normative_substrate_durability_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, weak_state_commitment_problem_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of durable honor cultures inherit a working reputation economy: recognized scripts for insult and redress, deterrence against predation and slander, and a settled ladder of standing that marriages, trade, and alliances ride on. In exchange they owe response when challenged and observance at ceremonies of reconciliation. Leaving would mean severing kin, congregation, and homeland at once, so departure is rare and costly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_community_members, beneficiary,
    organized, generational, identity_locked, regional).

% Commissioning oaths, academy honor codes, and regimental tradition give the corps discipline, internal trust, and a credible public promise that its word holds. Officers submit to honor boards and courts-martial that enforce truthfulness and conduct, accepting obligations far heavier than civilian peers carry. Exit means resigning a vocation, not merely a rule-set.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary,
    institutional, generational, constrained, global).

% Senior men preside over dispute settlement, certify apologies and reconciliations, and issue the standing judgments that arrange marriages and alliances. Deference flows to them; the physical answering of challenges falls to younger men. Their position inside the normative order is the most comfortable the order offers.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, status_elite_patriarchs, beneficiary,
    organized, generational, identity_locked, regional).

% Junior men carry the code's sharpest obligations: answer slights promptly or lose face, guard the household's standing, and show courage in feuds. Since prohibition, the law no longer shields the answers they give — prosecutions, prison terms, and ruined job prospects now land on them, while what compliance buys remains real: protection, respect, marriageability. Walking away would cost them their kin network and their name.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, young_men_in_honor_cultures, payer,
    moderate, immediate, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, young_men_in_honor_cultures, beneficiary).

% Legislatures criminalized dueling beginning in the late eighteenth century and tightened through the nineteenth; courts prosecuted principals and seconds; police broke up challenge rituals. The apparatus channeled disputes into libel, assault, and tort law instead. Once the practice thinned out, anti-dueling enforcement went largely dormant — the record shows the state could close the practice but never reached the normative layer beneath it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Universities, churches, licensed professions, and employers built the second wall: credentialing, ethics boards, dismissal for challengers, and liability regimes that made a duel an act of professional suicide. Each barrier raised the price of private satisfaction independently of anyone's feelings about honor, closing the practice from the top down.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, professional_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Women bound by the code's sexual double standard, clergy, pacifist-influenced men, and secularized youth live inside honor communities while rejecting satisfaction obligations. They enjoy none of the code's protections, cannot formally contest its judgments, and their objection carries no standing in the forums where the code is administered.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_code_refusers, excluded,
    powerless, biographical, trapped, regional).

% Historians of violence, legal historians, and quantitative criminologists reconstruct statute dates, duel-frequency series, and discourse traces to weigh exogenous suppression against endogenous change. They take no side in the honor economy; their instruments are archives, court records, and field experiments.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, violence_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__practice_decline_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__practice_decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the reputation-commitment problem of armed, status-bearing men in settings where courts are distant or distrusted: makes threats and promises credible by bonding them to standing, deters insult and predation in advance, and provides scripted, bounded dispute resolution — challenge, seconding, apology, reconciliation — that substitutes for adjudication.
% TRANSFER_FUNCTION: Moves obligation and risk downward and deference upward: the duty of answering insult concentrates on junior men who stake body, liberty, and livelihood; certified standing, marriage brokerage, and dispute-adjudication authority concentrate on senior men; historically, lethal risk transferred onto duel volunteers and their seconds.
% ABSENT_VOICES: Women bound by the code's sexual double standard, class-excluded men who suffered its violence without access to its protections, enslaved and colonized populations governed by honor regimes they could not invoke, and internal refusers — all absent from the codifying conversation, which was conducted by and for the gentlemen the code privileged.
% DISAPPEARANCE_RATIONALE: If the substrate vanished overnight, honor-region conduct would rearrange around the gap: insult scripts and feud sequences would lose their grammar, academy and regimental discipline would lose a load-bearing tradition, marriage-alliance brokerage would lose its certification machinery, and the scholarly contest this file belongs to would lose its subject. The reading's core assertion is precisely that live arrangements depend on this layer.
% FOUNDING_PROBLEM: In weak-state environments — frontier settlements, plantation colonies, officer corps stationed abroad — gentlemen needed credible commitments and insult-deterrence where courts were slow, corrupt, or absent and where equals could do one another lethal harm with impunity. The code made reputation a bondable asset and gave violence a rule-bound exit.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: legal historians document that dueling density tracked court weakness and fell as state adjudication penetrated; anthropological field experiments in honor regions attest that insult-response scripts still govern behavior where institutional trust is thin; military sociologists attest the disciplinary function of academy honor codes. No honor-community beneficiary attestation is relied on for the status judgment.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38 (interval end): the substrate still obliges its bearers — answer challenges, guard household standing, observe reconciliation forms — and in honor regions those obligations still purchase standing at real cost, but the lethal stakes that made the nineteenth-century code ruinous are gone; closure of the practice envelope lightened what membership costs. Suppression is 0.45 as a raw structural property, deliberately unscaled: it measures the code's own coercive hold on members (sanction, ostracism, kin pressure), not the state's parallel suppression of dueling, which belongs to a different constraint's ledger. Theater_ratio 0.32: ceremonial accretion is real (heritage rituals, commemorative duels, honor banquets) but the majority of substrate activity remains behavioral — insult scripts fire, honor boards sanction, feud logic organizes — so the arrangement is not mostly performance. Accessibility_collapse 0.30: dignity scripts, courts, and geographic exit remain genuinely available, which is exactly this reading's disagreement with the contraction sibling — had alternatives collapsed, the code would have transformed rather than persisted. Resistance 0.50: internal refusers, dignity movements, and two centuries of legal hostility meet active community defense. The temporal series run on one shared grid — twelve points at roughly decadal spacing from ~1800 (t0) to ~2020 (t22), every tracked metric authored at every point: base_extractiveness falls as the practice closes, theater_ratio rises gently as ceremony accretes onto a still-working core, and suppression_requirement declines as the code's enforcement shifted from violence-backed to social. That triple trajectory — falling bearer costs, stable norm recognition, mild theatrical accretion — is the signature this reading predicts for decline-without-delegitimation.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. The state and professional seats compute an arrangement that is effectively over: their enforcement object (the duel) vanished, their dockets emptied, and from their desks the honor world looks extinct. Community seats compute a living order: scripts, sanctions, and standing continue to organize conduct daily. Within the community, the junior male seat and the patriarch seat sit at opposite ends despite identical nominal membership — obligations and legal risk concentrate on juniors while deference concentrates on elders — and identity_lock keeps the gap from clearing through departure: honor is fused with self-concept, kin position, and place, so exit is experienced as self-annihilation rather than relocation, and the fusion would have to break (through sustained contact with dignity-frame institutions across a generation) before the asymmetry could dissolve. The engine computes these per-seat differences from the power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (honor_community_members, military_officer_corps, status_elite_patriarchs) derive directionality near the subsidized end: the code supplies their reputation order, discipline infrastructure, and deference at costs they largely externalize downward. young_men_in_honor_cultures carries the payer role with a beneficiary secondary role: derivation places them well toward the target end, amplified by identity_locked exit — they bear the concentrated obligations and, post-prohibition, the legal consequences the code no longer absorbs. The state_legal_apparatus and professional_institutions seats sit outside the substrate's gain-and-cost economy: they neither collect from its operation nor pay into it; their enforcement aims at the practice envelope, not at harvesting the code. violence_historians is the analytical seat. No directionality overrides are authored: the beneficiary-declaration-plus-exit derivation reproduces these positions, and the dual-role junior seat is the sharpest test the derivation faces. Receipt check behind the gain_flow authoring: no named seat demonstrably captures what the code still takes — obligations convert into diffuse order and standing across the community, with an elite skew in deference that falls short of demonstrable capture — so the receipt surface is authored as diffuse, an affirmative finding after checking each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two mislabels. Reading the empty dueling grounds as natural obsolescence — the code eroding like a spent law of nature — mistakes constructed enforcement for entropy; the record of staggered statutes and barrier-building shows deliberate closure, which is why the claim is rope rather than mountain. Reading the substrate's persistence as predatory endurance mistakes a thinning normative order for extraction someone is collecting; measured extraction fell as the practice closed, and no seat demonstrably captures what the code still takes. The founding problem — credible commitment where courts were thin — has receded where states consolidated but persists in honor regions with low institutional trust, so the mandate is contested rather than dead; no mandatrophy resolution is declared, and the R5 mismatch consumer reads contested status against a world_rearranges verdict without a zombie flag. On fixing cost: two centuries of state enforcement removed the practice without reaching the normative layer, which is direct evidence that no available lever removes the layer itself — fixing is prohibitive for any possible fixer, and the diffuse receipt plus prohibitive fixing combination is weighed by the engine against the live-function evidence (working insult scripts, enforced honor boards) rather than read as a vestige verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (practice_decline_reading) of the honor_satisfaction_substrate kernel; which structural features of the story would change under the sibling readings?',
    'Cross-reading corpus comparison: compile the cultural_contraction_reading and composite_overdetermined_reading files and diff their beneficiary structures, ε referents, and computed types against this file; run convergence tests on the shared evidence base (staggered statute timing, duel-frequency series, discourse corpora).',
    'If cultural_contraction is right, this story''s ε referent (a persisting substrate) misdescribes reality — the operative constraint would be a transformed dignity regime with different beneficiaries and near-zero extraction; if composite is right, this story understates endogenous causation and its rope claim needs amendment toward a hybrid structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed uncertainty: which reading of the honor kernel this file''s structure describes.').

omega_variable(
    exogenous_vs_endogenous_decline_share,
    'What share of dueling''s decline is attributable to exogenous enforcement (statutes, institutional barriers, opportunity cost) versus endogenous delegitimation of the honor code itself?',
    'Natural experiments from staggered anti-dueling statutes across jurisdictions; interrupted time-series on duel frequency around statutory and barrier dates; diaries and press tracing willingness-to-duel before versus after prohibition.',
    'A large endogenous share collapses this reading into the composite or contraction siblings and would re-date the substrate''s transformation earlier; a small share confirms exogenous sufficiency and stabilizes the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_decline_share, empirical, 'Causal weight split between external suppression and internal attitude change in the practice decline.').

omega_variable(
    substrate_vitality_vs_vestige,
    'Is the surviving honor material functional coordination (live insult scripts, working military honor enforcement) or inertial, performative residue?',
    'Field-experimental replication in honor regions (insult-response studies), cadet honor-system violation and sanction data, ethnographic audit of whether reconciliation ceremonies produce behavioral uptake or only performance.',
    'If vestigial, theater_ratio is understated and the arrangement drifts toward degraded-inertial dynamics (diffuse gains, prohibitive fixing, performative maintenance); if functional, the rope claim holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_vitality_vs_vestige, empirical, 'Whether the attenuated honor substrate still does work or is mostly kept for show.').

omega_variable(
    dueling_thinkability_status,
    'Does dueling remain thinkable within the honor framework — impractical but imaginable, as this reading holds — or has it become unthinkable, as the contraction sibling claims?',
    'Counterfactual endorsement elicitation in honor communities; documentation of enforcement-gap continuations and revivals (student fencing circuits, recorded nineteenth- and twentieth-century duels where institutional barriers lagged).',
    'Live thinkability supports this reading''s persistence claim and its rope framing; systematic refusal-to-entertain across all honor seats would shift the file toward the contraction sibling''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dueling_thinkability_status, conceptual, 'Whether the practice form remains imaginably available inside the honor framework.').

omega_variable(
    honor_suppression_mechanism_split,
    'Is the honor code''s remaining hold on its members structural (community sanction, kin-economic entanglement) or internalized (honor-as-self identity fusion that travels with the member after exit)?',
    'Post-exit trajectory of defectors: if obligation-intensity persists after geographic and economic exit from the honor community, the internalized share is substantial; if it decays quickly, the hold was structural.',
    'Internalized suppression raises effective suppression above the structural measure and strengthens the identity_locked exit coding; purely structural suppression would make pluralist exit policies more effective than they currently appear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_suppression_mechanism_split, empirical, 'Structural versus internalized split in the code''s coercive hold on members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hss_practice_decline_tr_t0, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t0, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t2, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 2, 0.14).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t2, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t4, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t4, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t6, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t6, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t8, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t8, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t10, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t10, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t12, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t12, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t14, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t14, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t16, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t16, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t18, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t18, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t20, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t20, observed).
narrative_ontology:measurement(hss_practice_decline_tr_t22, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 22, 0.32).
narrative_ontology:measurement_basis(hss_practice_decline_tr_t22, observed).

% Extraction over time
narrative_ontology:measurement(hss_practice_decline_be_t0, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(hss_practice_decline_be_t0, observed).
narrative_ontology:measurement(hss_practice_decline_be_t2, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 2, 0.54).
narrative_ontology:measurement_basis(hss_practice_decline_be_t2, observed).
narrative_ontology:measurement(hss_practice_decline_be_t4, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(hss_practice_decline_be_t4, observed).
narrative_ontology:measurement(hss_practice_decline_be_t6, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement_basis(hss_practice_decline_be_t6, observed).
narrative_ontology:measurement(hss_practice_decline_be_t8, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(hss_practice_decline_be_t8, observed).
narrative_ontology:measurement(hss_practice_decline_be_t10, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(hss_practice_decline_be_t10, observed).
narrative_ontology:measurement(hss_practice_decline_be_t12, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(hss_practice_decline_be_t12, observed).
narrative_ontology:measurement(hss_practice_decline_be_t14, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 14, 0.39).
narrative_ontology:measurement_basis(hss_practice_decline_be_t14, observed).
narrative_ontology:measurement(hss_practice_decline_be_t16, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement_basis(hss_practice_decline_be_t16, observed).
narrative_ontology:measurement(hss_practice_decline_be_t18, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement_basis(hss_practice_decline_be_t18, observed).
narrative_ontology:measurement(hss_practice_decline_be_t20, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(hss_practice_decline_be_t20, observed).
narrative_ontology:measurement(hss_practice_decline_be_t22, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 22, 0.38).
narrative_ontology:measurement_basis(hss_practice_decline_be_t22, observed).

% Suppression requirement over time
narrative_ontology:measurement(hss_practice_decline_su_t0, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(hss_practice_decline_su_t0, observed).
narrative_ontology:measurement(hss_practice_decline_su_t2, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement_basis(hss_practice_decline_su_t2, observed).
narrative_ontology:measurement(hss_practice_decline_su_t4, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 4, 0.67).
narrative_ontology:measurement_basis(hss_practice_decline_su_t4, observed).
narrative_ontology:measurement(hss_practice_decline_su_t6, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(hss_practice_decline_su_t6, observed).
narrative_ontology:measurement(hss_practice_decline_su_t8, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(hss_practice_decline_su_t8, observed).
narrative_ontology:measurement(hss_practice_decline_su_t10, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(hss_practice_decline_su_t10, observed).
narrative_ontology:measurement(hss_practice_decline_su_t12, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(hss_practice_decline_su_t12, observed).
narrative_ontology:measurement(hss_practice_decline_su_t14, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 14, 0.54).
narrative_ontology:measurement_basis(hss_practice_decline_su_t14, observed).
narrative_ontology:measurement(hss_practice_decline_su_t16, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement_basis(hss_practice_decline_su_t16, observed).
narrative_ontology:measurement(hss_practice_decline_su_t18, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(hss_practice_decline_su_t18, observed).
narrative_ontology:measurement(hss_practice_decline_su_t20, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(hss_practice_decline_su_t20, observed).
narrative_ontology:measurement(hss_practice_decline_su_t22, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 22, 0.45).
narrative_ontology:measurement_basis(hss_practice_decline_su_t22, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (honor_satisfaction_substrate), three readings, three files. This file decomposes the colloquial label 'the decline of dueling' into reading-indexed constraints per the ε-invariance principle: practice_decline_reading (this file — substrate persists, practice closed exogenously; ε authored over the persisting substrate), cultural_contraction_reading (the code transformed into dignity norms — a different ε referent: the transformed regime), composite_overdetermined_reading (joint causation — a blended referent). Edge logic: persistence evidence compiled here (staggered statute timing, enforcement-gap continuations, culture-of-honor field results) feeds the composite reading's causal weighting, hence the influences relation; the contraction reading competes on the same evidence with neither reading logically eliminating the other, hence coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
