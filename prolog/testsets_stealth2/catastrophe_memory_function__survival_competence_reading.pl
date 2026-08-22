% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Catastrophe-Commemoration Ritual as Survival-Competence Transmission (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   A geographically dispersed minority community maintains, in every willing
 *   household, an annual embodied re-enactment of its archetypal
 *   catastrophe-departure: the hurried meal, the travel food, the scripted
 *   questions that force each new generation to interrogate the story, the
 *   open door, the promise renewed at the table. The survival-competence
 *   reading holds that this standing arrangement is not primarily memorial
 *   but instructional — a decentralized, household-replicated curriculum that
 *   keeps departure-and-regrouping capability loadable under stress and
 *   survivable against the destruction of any central institution. KEY AGENTS
 *   (by structural relationship): - diaspora_jewish_households
 *   (moderate/constrained): participating population — runs the rehearsal,
 *   absorbs its costs, hands the script to its children. -
 *   observant_household_children (powerless/trapped): training recipients —
 *   receive the competence and bear its hours before they can consent. -
 *   rabbinic_authorities (institutional/identity_locked): agenda-setters —
 *   constitute the obligatory form; their office exists inside the practice.
 *   - rabbinic_educational_institutions (institutional/mobile): secondary
 *   collectors — package and sell the transmission infrastructure. -
 *   assimilated_descendants (moderate/mobile): excluded objectors — exited
 *   the practice, retained the ties. - ritual_theory_scholars
 *   (analytical/analytical): observers testing the efficacy premise. Per Rule
 *   1 this file authors only the survival-competence reading as a clean,
 *   epsilon-invariant constraint; the sibling readings are separate files
 *   linked through network.affects_constraints, and the committer structure
 *   is carried in omegas and in commentary.kernel_context.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.3).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Catastrophe-Commemoration Ritual as Survival-Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '50a41d62-9a1f-4290-8d51-579dfd05b083').
narrative_ontology:cs_kernel_codification('50a41d62-9a1f-4290-8d51-579dfd05b083', fixed_text).
narrative_ontology:cs_authority_grounding('50a41d62-9a1f-4290-8d51-579dfd05b083', lineage).
narrative_ontology:cs_interpretation_layer_present('50a41d62-9a1f-4290-8d51-579dfd05b083').
narrative_ontology:cs_reading_relation('50a41d62-9a1f-4290-8d51-579dfd05b083', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('50a41d62-9a1f-4290-8d51-579dfd05b083', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('50a41d62-9a1f-4290-8d51-579dfd05b083', foundational, embodied_rehearsal_transmits_operative_competence).
narrative_ontology:cs_axiom_status(embodied_rehearsal_transmits_operative_competence, holdable).
narrative_ontology:cs_axiom_grounding('50a41d62-9a1f-4290-8d51-579dfd05b083', embodied_rehearsal_transmits_operative_competence, empirically_contingent).
narrative_ontology:cs_axiom('50a41d62-9a1f-4290-8d51-579dfd05b083', secondary, decentralized_household_replication_preserves_chain_against_node_destruction).
narrative_ontology:cs_axiom_status(decentralized_household_replication_preserves_chain_against_node_destruction, holdable).
narrative_ontology:cs_axiom_grounding('50a41d62-9a1f-4290-8d51-579dfd05b083', decentralized_household_replication_preserves_chain_against_node_destruction, instrumental).
narrative_ontology:cs_reference_frame('50a41d62-9a1f-4290-8d51-579dfd05b083', portable_survival_curriculum).
narrative_ontology:cs_drift_state('50a41d62-9a1f-4290-8d51-579dfd05b083', contemporary_assimilation_revival_cycle, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('50a41d62-9a1f-4290-8d51-579dfd05b083', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, observant_household_children).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, rabbinic_educational_institutions).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, rabbinic_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, observant_household_children).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, catastrophe_recurrence_preparedness_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, embodied_ritual_transmission_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the annual commemorative meal in which the departure from catastrophe is narrated and physically enacted: unleavened bread eaten as travelers eat, door opened for the hidden arrival, children prompted to ask why this night differs. Each year the household spends preparation time and holiday budget and receives back a working copy of the community's departure-and-regrouping script, installed in its children. Opting out entirely is possible but carries family friction, marriage-market effects, and a sense of severing the chain the household inherited.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households, beneficiary,
    moderate, generational, constrained, global).

% Are drilled annually in the questions, foods, songs, and sequence long before they can evaluate what is being taught. They receive the transmitted procedure — how a dispersed people leaves quickly, carries little, rebuilds institutions wherever it lands — and they pay for it in autonomy and holiday hours they did not choose to surrender. Their exit from the training is not available to them while they remain in the household; what they can do later, as adults, is a separate decision.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, observant_household_children, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, observant_household_children, payer).

% Define the obligatory form of the commemoration, adjudicate disputed practice, certify correct performance, and train the successors who will do the same. Their standing is constituted by the practice continuing: an authority that presides over the transmission cannot walk away from it without dissolving the office itself. They collect legitimacy, deference, and employment from the arrangement's continuation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, rabbinic_authorities, beneficiary).

% Package the transmission infrastructure: printed and digital retellings, school curricula, museum programs, packaged foods certified for the occasion. Revenue and enrollment depend on continued household demand for the annual observance, so they amplify the survival-competence framing in their materials. Unlike the clerical authorities they can redeploy staff and capital into adjacent programming if demand shifts.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, rabbinic_educational_institutions, beneficiary,
    institutional, generational, mobile, global).

% Have already stepped out of regular observance while keeping family ties inside practicing networks. Each spring they absorb invitations, expectations, and occasional reproach. Their standing objection — that the competence can be kept by secular means: history teaching, emergency planning, diaspora institutions without liturgy — is rarely voiced inside communal forums, where their absence from the table is treated as having answered the question.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, assimilated_descendants, excluded,
    moderate, biographical, mobile, continental).

% Study whether commemorative practice correlates with measurable group outcomes: survival rates under persecution, speed of community reconstitution after displacement, retention of languages and trade skills across generations. They produce the evidence base on which the survival-competence claim stands or falls, and they owe nothing to either the authorities or the households.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_theory_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__survival_competence_reading, rabbinic_educational_institutions).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an intergenerational knowledge-transfer problem no individual family can solve alone: how a dispersed people keeps departure-and-regrouping capability alive across centuries, when written instructions decay, motivation to learn unused procedures fades, and any centralized store of the knowledge presents a single point of destruction. A standardized annual embodied rehearsal, replicated in every household, keeps the procedure loadable under stress and keeps the chain of transmission alive without depending on any institution surviving.
% TRANSFER_FUNCTION: Moves time, attention, and household resources from every generation of participants into the shared rehearsal; moves procedural memory and the departure narrative from elders to children; moves legitimacy and deference to the clerical authorities who certify performance; moves a smaller money stream to the publishing, education, and certified-food economy that surrounds the observance.
% ABSENT_VOICES: Assimilated descendants who would argue the competence transmits without liturgy, secular-preparedness advocates who maintain comparable capability through explicit planning, and historians skeptical of any causal link between ritual depth and measured survival. All three sit outside the communal forums where the practice's necessity is affirmed; their absence lets unanimity stand untested.
% DISAPPEARANCE_RATIONALE: If the annual rehearsal vanished overnight, a transmission channel closes that nothing currently replaces at scale: children stop acquiring the departure script, the household-level redundancy that survived the destruction of every central institution lapses, the clerical offices lose the practice that constitutes them, and the surrounding educational economy loses its anchor product. Participating populations would reorganize around whatever partial substitutes — schooling, museums, family memory — they could improvise.
% FOUNDING_PROBLEM: After catastrophic displacement, how does a people with no state and no secure territory carry, across generations of dispersion, the practical capacity to flee quickly, regroup, and rebuild institutions wherever it lands — without relying on any single archive, authority, or territory that the next catastrophe could erase?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the historical-demographic record of recurrent expulsion and mass violence against diaspora populations, and by disaster-sociology findings on the premium that decentralized, pre-networked groups show in crisis response. Corroboration is partial and contested: genocide-studies literature also documents large survival differences unrelated to observance depth, and survivor testimony includes many who credit secular networks, languages, and luck — so the problem is attested as live by independent sources while the claim that THIS arrangement solves it is disputed by independent sources.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.30 at interval end): the standing arrangement costs each household real annual time and budget and streams revenue to the surrounding institutional economy, but the dominant flow runs toward the participants themselves in the form of transmitted procedure, and no seat captures gains disproportionate to the function delivered. Suppression (0.28) reflects soft communal enforcement — expectation, reproach, marriage-market effects — not a coercive apparatus; note suppression is a raw structural property, unscaled by power or scope, while effective extraction is computed by the engine from directionality and the global spatial scale. Accessibility collapse is low (0.30): workable alternatives to ritual transmission exist (explicit history curricula, secular preparedness planning, museum education, family storytelling) and remain live options, which is what distinguishes this from a natural-law profile. Resistance (0.30) records the sustained reformist and assimilationist drag on obligatory framing. The temporal series run on ONE shared eleven-point grid (every tracked metric authored at every decade) so no end-state value leaks backward into earlier rows. The theater_ratio series is deliberately cyclical — one full rise-fall-partial-rise cycle across the interval — tracking the assimilation-era hollowing of the practice into rote performance (peak ~0.48 mid-interval) followed by identity revival that re-functionalized it, with a recent mild climb as heritage packaging grows; the oscillation is driven by the external environment (threat salience versus integration comfort), not by the arrangement itself, and unlike intermittent-reinforcement cases the cycling is not here the extraction mechanism. Base_properties are sampled at the end-state phase (t=100), post-revival, mid-cycle. The slow climb in base_extractiveness (0.18 to 0.30) models layered institutional monetization around a stable core — worth watching, nowhere near capture.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is arranged so they do. From the rabbinic-authority seat the arrangement is the coordination it personally embodies and perpetuates — near-beneficiary directionality, identity-locked exit, so the computed classification from that seat will sit at the cooperative end regardless of the authored claim. From the household seat the same structure reads as inherited duty balanced by received capability — near-symmetric. From the child seat, with the directionality override raising d to 0.45 and exit locked at trapped, the computed extraction is amplified: a powerless agent at global scope, trained without consent, experiences the constraint's costs at nearly full weight while the competence it buys is unverifiable until a catastrophe arrives. Two institutional seats at identical nominal power differentiate cleanly on exit options alone: the clerical authority cannot leave the practice without dissolving the office (identity_locked), while the educational industry can redeploy its capital into adjacent programming (mobile) — same power atom, opposite exit atoms, different computed seats. The engine owns these computations; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: diaspora_jewish_households and observant_household_children are declared beneficiaries and derive low directionality; rabbinic_authorities and rabbinic_educational_institutions collect legitimacy and revenue and likewise sit toward the beneficiary end, the latter with arbitrage-grade exit pushing it nearest zero. No victim class is declared under this reading, and none is structural to it: the arrangement's costs are participation costs borne roughly by those who receive the transmitted good. The single directionality_override corrects the one place the derivation misfires: the children are declared beneficiaries and would derive deep-beneficiary directionality, but their actual position is near-symmetric (d 0.45) because they bear the training's full cost in autonomy and hours BEFORE they can evaluate the benefit, and they cannot decline it. The derivation reads their role; the override reads their position. The gendered preparation-labor asymmetry is deliberately NOT folded in here — see the gendered_prep_labor_boundary omega: under epsilon-invariance discipline it is authored as a candidate sibling constraint, and if the decomposition test ever rules it internal, this file gains a victim set and the tangled_rope gate opens.
 *
 * MANDATROPHY ANALYSIS:
 *   The decomposition is what prevents mislabeling in both directions. Read cynically across the whole kernel, the practice looks like a snare (obligation enforced on the young, exits socially taxed, an authority class collecting deference) — but that profile belongs chiefly to the sibling mourning/boundary reading, where identity enforcement is the point; importing it here would convict the survival-curriculum function for another constraint's crimes. Read naively on its own metrics, the arrangement looks like a pure coordination win — but the theater cycle and the slow extraction climb are exactly the drift signals the lifecycle detector exists to catch, and the receipt surface (partial institutional capture) is flagged rather than denied. Mandatrophy status: the founding problem is live (attested independently, see corroboration), the function is substantially intact, theater_ratio at end-state (0.34) sits well below the degraded-performance threshold, and no concentrated capturer has grown large enough to convert coordination into cover. The R5 mismatch check aligns: founding_problem_status=live crossed with disappearance_verdict=world_rearranges produces no zombie flag. Fixing cost is authored prohibitive: replacing the embodied chain with an explicit secular curriculum is technically conceivable, but for whoever could fix it (communal leadership) the swap would forfeit the lineage-legitimacy their authority rests on, and for participants it would break an identity-infrastructure whose replacement value nobody has demonstrated exceeds the switching cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint instantiates one reading (survival_competence, D5) of the catastrophe_memory_function kernel. Would adopting a sibling reading change the epsilon and party structure?',
    'Compare the compiled sibling files: the mourning_practice reading centers boundary-norm enforcement and should author materially higher epsilon with a deviant-class victim set; the hybrid reading should author an intermediate profile combining both structures.',
    'If the mourning reading is adopted as the operative account, this practice''s epsilon rises sharply (obligation enforced against deviants) and a victim class appears; under the hybrid reading the extraction sits between. The disagreement is located in what the ritual''s persisting core function IS, not in any observable parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame omega: reading-indexed epsilon over a shared kernel; sibling files carry the other readings.').

omega_variable(
    embodied_transmission_efficacy,
    'Does embodied annual rehearsal actually transmit operative survival competence, or does it transmit memory-performance that substitutes for concrete capability?',
    'Comparative disaster-sociology and genocide-studies research correlating depth of ritual observance with measured crisis outcomes: evacuation compliance, mutual-aid mobilization speed, multilingual capacity, document and fund readiness, network density under displacement.',
    'If rehearsal is efficacious, the coordination function is real and the rope reading strengthens. If it transmits only the performance of memory, the constraint''s function is largely theatrical and it drifts toward piton with rising effective cost per participant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_transmission_efficacy, empirical, 'Whether the transmission mechanism delivers capability or its simulacrum.').

omega_variable(
    preparedness_substitution_effect,
    'Does the ritual crowd out material preparedness (emergency savings, documents, language skills, exit networks) by giving participants a felt sense of readiness that is never tested?',
    'Within-community comparison of households matched on income and geography but differing in observance depth, scored on concrete readiness inventories rather than self-reported preparedness.',
    'If substitution is real, the arrangement imposes costs while suppressing the alternative behavior it nominally serves — the strongest available route by which this reading degrades from rope toward snare-flavored operation. If absent or negative (ritual correlates with MORE material preparation), the reading is confirmed robustly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preparedness_substitution_effect, empirical, 'Whether symbolic readiness displaces material readiness.').

omega_variable(
    internalized_obligation_suppression,
    'Is the residual suppression (0.28 at interval end) structural (communal sanction, family pressure, marriage-market effects) or internalized (guilt, filial obligation, identity fusion that persists after external barriers fall)?',
    'Post-exit suppression trajectory: track individuals who leave observance; if holiday-season distress and compelled re-participation persist after leaving dense communal environments, a substantial share is internalized.',
    'If mostly internalized, effective suppression for trapped seats exceeds the structural measure and exit-option atoms understate lock-in for the next generation; if mostly structural, pluralistic geographic mobility should continue eroding it along the observed decay curve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_obligation_suppression, empirical, 'Structural versus internalized split of the remaining enforcement burden.').

omega_variable(
    gendered_prep_labor_boundary,
    'Is the historically gendered distribution of preparation labor inside the standing arrangement part of THIS constraint (survival-competence transmission), or a separable household-labor constraint that merely coincides with it?',
    'Decomposition test: measure transmission efficacy and participation costs in communities using rotated/shared preparation norms. If transmission outcomes are unchanged while the labor asymmetry vanishes, the asymmetry is a separate constraint and belongs in its own file.',
    'If the asymmetry is ruled internal, a victim class enters this story and the tangled_rope gate opens (coordination plus asymmetric cost-bearing). Under the decomposition adopted here it is authored as a sibling constraint; this story stays clean with no victims declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_prep_labor_boundary, conceptual, 'Epsilon-invariance boundary decision: which costs belong inside this constraint''s referent.').

omega_variable(
    catastrophe_risk_calibration,
    'Is the diaspora-catastrophe recurrence risk, on which the annual cost of rehearsal is justified, actually calibrated such that the maintenance cost is proportionate?',
    'Actuarial reconstruction of realized catastrophe frequency against the precaution level the practice embodies, combined with explicit decision-theoretic valuation of option preservation for low-frequency high-severity events.',
    'If recurrence risk is judged negligible, the practice''s ongoing costs become tradition-weight without warrant and the constraint drifts toward piton (maintained theatrically by inertia). If risk is material, the cost is insurance premium and the rope reading holds across the whole interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_risk_calibration, empirical, 'Whether the risk premise sustaining the cost side of the ledger is calibrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_mem_survcomp_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cat_mem_survcomp_tr_t10, catastrophe_memory_function__survival_competence_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cat_mem_survcomp_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cat_mem_survcomp_tr_t30, catastrophe_memory_function__survival_competence_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(cat_mem_survcomp_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(cat_mem_survcomp_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(cat_mem_survcomp_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(cat_mem_survcomp_tr_t70, catastrophe_memory_function__survival_competence_reading, theater_ratio, 70, 0.26).
narrative_ontology:measurement(cat_mem_survcomp_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(cat_mem_survcomp_tr_t90, catastrophe_memory_function__survival_competence_reading, theater_ratio, 90, 0.32).
narrative_ontology:measurement(cat_mem_survcomp_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.34).

% Extraction over time
narrative_ontology:measurement(cat_mem_survcomp_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cat_mem_survcomp_be_t10, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(cat_mem_survcomp_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(cat_mem_survcomp_be_t30, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(cat_mem_survcomp_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(cat_mem_survcomp_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.24).
narrative_ontology:measurement(cat_mem_survcomp_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(cat_mem_survcomp_be_t70, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 70, 0.23).
narrative_ontology:measurement(cat_mem_survcomp_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(cat_mem_survcomp_be_t90, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 90, 0.28).
narrative_ontology:measurement(cat_mem_survcomp_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cat_mem_survcomp_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cat_mem_survcomp_su_t10, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(cat_mem_survcomp_su_t20, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(cat_mem_survcomp_su_t30, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(cat_mem_survcomp_su_t40, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(cat_mem_survcomp_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.33).
narrative_ontology:measurement(cat_mem_survcomp_su_t60, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 60, 0.31).
narrative_ontology:measurement(cat_mem_survcomp_su_t70, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 70, 0.3).
narrative_ontology:measurement(cat_mem_survcomp_su_t80, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 80, 0.29).
narrative_ontology:measurement(cat_mem_survcomp_su_t90, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 90, 0.28).
narrative_ontology:measurement(cat_mem_survcomp_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, information_standard).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'catastrophe commemoration ritual' covers structurally distinct claims with distinct epsilons. This file authors the D5 claim (survival-competence transmission; low extraction, no victim class, rope-profile). The sibling file authors the D1/D4 claim (memorial obligation and boundary-norm maintenance; higher extraction, a deviant-class victim set, enforcement-dependent). The hybrid file authors the conjunction. The upstream/downstream gradient runs from this reading outward: efficacy evidence assembled under the survival-competence claim is routinely cited as partial warrant for the hybrid synthesis, so this story links to both siblings. Epsilon values differ ACROSS the family because the referent differs per reading — each file assesses the same standing arrangement by its own reading's lights; no file hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
