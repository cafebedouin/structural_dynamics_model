% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Trap
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The Lycurgan laws of ancient Sparta claimed to be unchangeable, divinely
 *   ordained ordinances establishing equality among citizens through
 *   inalienable land plots (kleroi) and collective dining (syssitia). By the
 *   4th–3rd century BCE, this immutability doctrine functioned as a snare
 *   preventing revision of property law, citizenship criteria, and helot
 *   status. The rigid code masked wealth consolidation by landed families
 *   (through legal loopholes in adoption and marriage) and prevented
 *   adaptation to demographic collapse. Younger Spartiate cohorts unable to
 *   access kleroi plots were excluded from full citizenship despite nominal
 *   eligibility. The unchangeable law perpetuated helot subjection as part of
 *   the sacred order. Attempts at reform (Agis IV, Cleomenes III) were
 *   suppressed by execution and exile, justified on grounds that revision
 *   would violate the ancestral compact. The system operated on a steady
 *   trajectory toward demographic and political collapse: extractiveness rose
 *   from 0.62 (early classical period) to 0.89 (Hellenistic period); theater
 *   ratio climbed from 0.25 to 0.68, indicating an increasingly performative
 *   maintenance of the unchangeability doctrine while actual legal practice
 *   accommodated selective accommodations favoring wealth concentration. The
 *   founding problem (aristocratic tyranny) was dead by the 4th century; the
 *   immutability doctrine then functioned purely to prevent collective
 *   adaptation to the new problem (population decline). This reading
 *   instantiates the Lycurgan kernel as a DEMOGRAPHIC TRAP: the
 *   irreversibility mechanism itself became the vector of collapse.
 *
 * KEY AGENTS:
 *   - Ephorate apparatus: institutional agenda-setter enforcing unchangeability doctrine; authority derives from being trustees of fixed law; any willingness to revise dissolves their legitimacy.
 *   - Spartiate citizen body: moderate-power payers, identity-locked through collective identity; constrained by kleroi system and syssitia participation; prevented from voting to revise the laws that constrain them.
 *   - Landed aristocracy: powerful beneficiaries; accumulate wealth through legal mechanisms the unchangeable code permits (adoption, marriage); immutability shields them from redistribution reform.
 *   - Helot underclass: powerless payers; bound to kleroi and excluded from personhood; unchangeable law perpetuates their subjection as sacred order.
 *   - Younger Spartiate cohorts: powerless payers, identity-locked; unable to obtain kleroi due to consolidation and scarcity; face social degradation or mercenary exit.
 *   - Reformist factions: excluded; recognize demographic spiral; suppressed by ephorate (execution, exile); marginalized by immutability doctrine.
 *   - Historical observers: analytical seat; document the contradiction between immutability doctrine and selective legal change; witness the system's failure to solve its own stated problems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.89).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.92).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.94).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '27986049-8beb-4b8c-8374-f93740ebb504').
narrative_ontology:cs_kernel_codification('27986049-8beb-4b8c-8374-f93740ebb504', fixed_text).
narrative_ontology:cs_authority_grounding('27986049-8beb-4b8c-8374-f93740ebb504', extraction).
narrative_ontology:cs_interpretation_layer_present('27986049-8beb-4b8c-8374-f93740ebb504').
narrative_ontology:cs_reading_relation('27986049-8beb-4b8c-8374-f93740ebb504', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('27986049-8beb-4b8c-8374-f93740ebb504', lycurgan_laws__adaptive_fiction_reading, influences).
narrative_ontology:cs_axiom('27986049-8beb-4b8c-8374-f93740ebb504', foundational, immutability_prevents_adaptation).
narrative_ontology:cs_axiom_status(immutability_prevents_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('27986049-8beb-4b8c-8374-f93740ebb504', immutability_prevents_adaptation, empirically_contingent).
narrative_ontology:cs_axiom('27986049-8beb-4b8c-8374-f93740ebb504', secondary, founding_problem_obsolescence_trap).
narrative_ontology:cs_axiom_status(founding_problem_obsolescence_trap, holdable).
narrative_ontology:cs_axiom_grounding('27986049-8beb-4b8c-8374-f93740ebb504', founding_problem_obsolescence_trap, empirically_contingent).
narrative_ontology:cs_reference_frame('27986049-8beb-4b8c-8374-f93740ebb504', ancestral_equality_through_unchanging_law).
narrative_ontology:cs_drift_state('27986049-8beb-4b8c-8374-f93740ebb504', hellenistic_period_crisis, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('27986049-8beb-4b8c-8374-f93740ebb504', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, ephorate_apparatus).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, landed_aristocracy).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_citizen_body).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helot_underclass).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, younger_spartiate_cohorts).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, constitutional_immutability_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, unchangeable_ancestral_law_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five annual magistrates who interpret and enforce Lycurgan law. Their authority derives explicitly from the unchangeability of the system — the doctrine that they administer a fixed, sacral code. Any willingness to revise that code would dissolve their legitimacy as trustees of the ancient order. They resist reform proposals, claiming fidelity to the ancestors, yet the system's rigidity increasingly contradicts their core function: adaptive governance to sustain the polis.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephorate_apparatus, agenda_setter,
    institutional, generational, trapped, local).

% Full citizens bound by the kleroi system (inalienable land plots) and mandatory participation in communal dining (syssitia). Their identity is constituted through the Spartan collective — exit would mean ceasing to be Spartiate. The immutability doctrine prevents them from voting to revise the laws that constrain them: citizenship is explicitly contingent on acceptance of unchangeable law. Over generations, demographic decline (fewer sons, more female-line inheritance, reduced reproduction) creates population stress that cannot be addressed through legal revision.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_citizen_body, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, spartiate_citizen_body, excluded).

% Ancient landowning families who hold productive kleros plots nominally inalienable but consolidate them through marriage and adoption (legal mechanisms the unchangeable code does contain). The rigid property system, paradoxically, creates opportunities for the wealthy to accumulate while appearing to honor the ancestral order. The immutability doctrine shields their accumulation from redistribution reform.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, landed_aristocracy, beneficiary,
    powerful, generational, constrained, local).

% Bound to the kleros they work; systematically excluded from citizenship and legal personhood. The unchangeable law perpetuates their subjection as part of the sacred order. No mechanism exists within the constitutional framework to improve their status; the immutability doctrine prevents helots and Spartiate reformers alike from revising the legal basis of their exploitation.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_underclass, payer,
    powerless, biographical, trapped, local).

% Young men born into declining family lines, unable to obtain a kleros plot because population-wide scarcity has reduced available land and inheritance by primogeniture concentrates plots. The unchangeable laws prevent creation of new citizenship pathways or alternative economic participation. These men are simultaneously citizens (by birth) and excluded (unable to meet the economic prerequisites for full participation). They face social degradation or exit to become foreign mercenaries, effectively severance from the polis.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, younger_spartiate_cohorts, payer,
    powerless, biographical, identity_locked, local).

% Citizens who recognize the demographic spiral and propose revisions to property law, syssitia participation, or citizenship criteria. They are marginalized within the system because the immutability doctrine defines reform as betrayal. Their proposals are ruled out of order by the ephorate as violations of the ancestral compact. Historical figures like Agis IV and Cleomenes III attempted reform but were executed or exiled — the system enforces its own immutability through violence against internal dissenters.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, reformist_spartiate_faction, excluded,
    moderate, biographical, constrained, local).

% Later historians (Plutarch, Aristotle) document the system's operation and decline. They witness the contradiction between the immutability doctrine and the reality that the code admits interpretation and selective change (land consolidation, adoption law expansion, citizenship-grade variation). The observers see that the unrevisability principle masks selective enforcement favoring the wealthy while preventing collective adaptation.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, historical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, landed_aristocracy).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains internal equality and political cohesion among Spartiate citizens through inalienable land plots and collective dining; prevents aristocratic landed consolidation (ostensibly); unifies the military through shared upbringing and identity.
% TRANSFER_FUNCTION: Extracts labor value from the helot underclass, who work kleroi plots and finance Spartiate leisure, military training, and syssitia participation. Transfers population pressure and economic constraint from the aggregate to younger cohorts excluded from land access. Transfers interpretive authority from the citizen assembly to the ephorate by classifying all law as unchangeable — preventing collective revision of property, citizenship, or helot status.
% ABSENT_VOICES: Helots are structurally excluded from legal voice and cannot advocate for revision of their status. Younger Spartiate cohorts lack formal standing to propose constitutional reform. Foreign observers and rival Greek poleis recognize the system's contradictions but have no legal standing in Spartan deliberation. Reformist factions are episodically present but systematically marginalized by the immutability doctrine.
% DISAPPEARANCE_RATIONALE: If the Lycurgan immutability doctrine disappeared, Sparta would face immediate pressure to revise property law (redistribution of accumulated kleros plots), expand citizenship eligibility (absorbing younger excluded cohorts), and modify or abolish helot subjection (at minimum, allowing manumission and legal status revision). The political economy of the polis depended entirely on the unchangeability claim to prevent these revisions. Without it, internal coalitions for reform would crystallize and the ancien régime structure would collapse into a revised constitutional order.
% FOUNDING_PROBLEM: Arose in the 9th–8th century BCE as Sparta transitioned from monarchical tribal organization to a more complex polis structure. The legendary Lycurgus (probably a composite or mythological figure) is credited with imposing a comprehensive code of laws designed to ensure equality among landholders, prevent tyranny through aristocratic consolidation, and unify the warrior-citizen body through shared discipline and collective dining. The founding problem was political stability amid growth and heterogeneity.
% FOUNDING_PROBLEM_CORROBORATION: Plutarch (1st century CE) and later historians attest that by the 4th–3rd century BCE, the founding problem of aristocratic tyranny had transformed into an entirely different problem: demographic collapse and wealth concentration. Aristotle (4th century BCE) explicitly critiques the system as failing its own stated goal — the unchangeable code actually produced accumulation and inequality. External observers (visiting Greeks, Roman historians) document that the real 4th–3rd century pressure was population decline and land scarcity, not the ancient fear of tyrants. Spartiate reformers themselves (Agis IV, Cleomenes III, 3rd century BCE) appealed to the founding problem (restore the ancestral system) while actually attempting to solve a new problem (revise property law to address demographic crisis) — their invocation of Lycurgus PROVED the founding problem was no longer live.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness of 0.89 reflects the magnitude of asymmetric extraction: the unchangeable law prevents collective revision benefiting the payer seats (Spartiate citizens, helots) while enabling wealth consolidation and maintained subjection. Suppression of 0.92 is extraordinarily high because the constraint's persistence depends on preventing revision through institutional veto (ephorate), violence (execution of reformers), and internalized identity lock (citizenship contingent on accepting unchangeable law). Theater ratio of 0.68 indicates the constraint's function has fundamentally shifted: early on, the unchangeable law genuinely coordinated against aristocratic tyranny; by the Hellenistic period, most enforcement activity maintains the performance of unchangeability while actual legal practice selectively accommodates wealth accumulation. The measurement series spans 400 years, with extractiveness and theater both rising monotonically while suppression intensifies to maintain a system no longer solving its original problem. This arc is diagnostic of a snare: the immutability doctrine that once served coordination became the mechanism of extraction. Accessibility collapse of 0.94 reflects the near-total elimination of alternatives: Spartiate citizens cannot revise the law through any constitutional mechanism; helots cannot appeal to legal personhood; younger cohorts cannot create new citizenship pathways; reformist factions face execution. Resistance of 0.31 is relatively low not because the payers accept the system, but because the suppression machinery prevents organized resistance from crystallizing (marginalizing reformists, executing reform leaders, maintaining the identity-lock). The constraint operates by pre-empting collective action rather than by convincing the payers of its legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the ephorate's seat: the immutability doctrine is a sacred trust, a guarantee of stability, and a shield against corruption — the constraint is genuine coordination. From the Spartiate citizen body's seat: the unchangeability is a trap preventing them from voting to revise property law and membership criteria — the constraint is suppression. From the landed aristocracy's seat: the unchangeable code is leverage, permitting selective interpretation that consolidates wealth while blocking redistribution — the constraint is beneficial extraction. From helots' seat: unchangeability perpetuates their subjection — the constraint is pure victimization. From younger Spartiate cohorts' seat: the law prevents alternative pathways to citizenship — the constraint is exclusion. From reformist factions' seat: the doctrine is a cage, preventing necessary adaptation — the constraint is institutional death spiral. The engine should compute these seats' classifications differently based on directionality: agenda-setter and landed aristocracy would compute as beneficiaries (low d, negative χ); citizen body and younger cohorts as targets (high d, high χ); helots as fully targeted (d near 1.0). The ephorate's power (institutional) and exit (trapped — revision would dissolve their authority) produce high d toward the beneficial side despite nominal governance role, because their structural relationship is preserving a system that benefits them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is heterogeneous across seats due to power and exit_options divergence. The ephorate (institutional power, trapped exit) has low d — they are structurally beneficiaries, deriving authority from the system's immutability. Landed aristocracy (powerful, constrained exit) has low d — they benefit from selective enforcement. Spartiate citizens (moderate power, identity_locked exit) have high d — they are constrained by the unchangeable law and unable to exit without ceasing to be Spartiate. Younger cohorts (powerless, identity_locked exit) have d near 1.0 — they bear the exclusionary constraint and cannot exit. Helots (powerless, trapped exit) have d at 1.0 — they are fully targeted; they bear legal subjection and cannot exit. Reformist factions (moderate power, constrained exit) have high d — they are targeted by suppression (execution, exile, marginalization). The divergence arises structurally: the same law produces opposite directionality for those who control its interpretation (ephorate, landed aristocracy) versus those bound by its rigidity (younger Spartiate, helots). This is the seat divergence that makes the constraint a snare from most seats' perspective and a beneficial arrangement from the beneficiary seats' perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was aristocratic tyranny; the mandate was to ensure equality among citizens through inalienable property and collective identity. By the 4th–3rd century BCE, this mandate was dead: Sparta faced demographic collapse, not aristocratic over-concentration of power. The immutability doctrine, which once enforced the mandate (preventing revision toward privatization), now enforced the opposite — it prevented revision toward demographic adaptation. This is classic mandate obsolescence: the constraint persists and even intensifies its suppression machinery, but it no longer solves the problem it was built to solve. The 'founding_problem_status: dead' classification combined with 'disappearance_verdict: world_rearranges' triggers the mismatch that mandatrophy detection should flag. The reformist proposals of Agis IV and Cleomenes III attempted to revise property law to address the demographic crisis while appealing to Lycurgan fidelity — they tried to separate the mandate (citizen equality and polis stability) from the specific mechanism (unchangeable kleroi law) that had become counterproductive. Their suppression and execution confirm that the system's administrators prioritized the irreversibility mechanism over the original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretation_contest,
    'Is the Lycurgan immutability doctrine a genuine constitutional principle that Spartiate leadership believed and enforced, or a legitimation myth layered over covert adaptation?',
    'Textual analysis of ephorate decisions and helot/property law changes; compare explicit claimed immutability against documented legal variations. If variations cluster around land consolidation and exclude helot status revision, the doctrine masks selective enforcement favoring the wealthy.',
    'If genuine doctrine, the immutability suppression is structural (the system genuinely forbade collective revision). If myth, the suppression is theatrical — the real mechanism is institutional power (ephorate + wealthy families) enforcing their preferred outcomes under cover of unchangeability. The theta reading would classify as adaptive_fiction; this reading (demographic_trap) requires genuine doctrine that blocks necessary adaptation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretation_contest, empirical, 'Whether immutability was a constitutive principle or a cover story for selective enforcement.').

omega_variable(
    demographic_causation,
    'Did the unchangeable Lycurgan laws directly cause demographic collapse, or did external factors (war, plague, economic competition with other poleis) drive population decline, which then exposed the rigidity of the system?',
    'Comparative analysis: did other Greek poleis with revisable constitutions experience similar demographic decline? Did Sparta''s decline correlate specifically with refusals to revise property and citizenship law (testable via reform proposals and ephorate rejection dates)? Or did Sparta decline for exogenous reasons, with rigidity being a symptom, not the cause?',
    'If causation runs Lycurgan_rigidity → demographic_collapse, the snare classification holds. If external shocks caused collapse and rigidity merely prevented adaptation, the constraint becomes a Piton (inertial persistence of a function that once served coordination). If both, the question is proportionality: how much of the collapse is attributable to each mechanism?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_causation, empirical, 'Whether immutability was causal driver or symptom of decline.').

omega_variable(
    reformist_suppression_mechanism,
    'What suppression mechanisms prevented reformist Spartiate factions from revising the law? Was it violence (executions, exile of reformers), institutional lock (ephorate veto authority), or internalized identity fusion (reformers themselves accepted that changing the law meant ceasing to be truly Spartiate)?',
    'Case study of Agis IV and Cleomenes III reform attempts: examine the official justifications for their execution/exile, the resistance they faced, and whether other Spartiate factions actively opposed them or were prevented from supporting them. Assess contemporary rhetoric: did reformers frame their proposals as compatible with Lycurgus (suggesting identity-lock) or as necessary breaks from the ancestral order (suggesting they had overcome identity fusion)?',
    'If suppression was primarily structural (violence, institutional veto), the snare classification is robust. If primarily internalized (identity-lock), the snare persists but with a different mechanism — the payers carry the suppression with them even if the institutional framework changed. If mixed (both structural and internalized), the post-exit scenario for reformers would show residual suppression, suggesting deep institutional-identity coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_suppression_mechanism, empirical, 'Whether suppression was structural coercion or internalized identity lock.').

omega_variable(
    competing_readings_simultaneity,
    'Can the sacral_fidelity_reading (immutability as sacred, divinely ordained) and the demographic_trap_reading (immutability as a snare that caused collapse) coexist as simultaneously held readings within the historical Spartan political discourse?',
    'Historical textual evidence: did Spartiates at different moments, or different factions, hold both readings? Did reformers cite demographic crisis while also claiming fidelity to Lycurgus (suggesting they distinguished the sacred principle from its disastrous application)? Or were the readings mutually exclusive, with one faction wholly embracing sanctity and another wholly embracing the collapse critique?',
    'If the readings coexist, the constraint is a Tangled Rope from some seats'' perspective (genuine coordination function + sacred fidelity + demographic necessity to revise) and a Snare from others (pure extraction via unchangeable law, regardless of founding function). If mutually exclusive, one reading is foreclosed by the other''s core premise, suggesting a genuine logical conflict rather than perspectival divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_readings_simultaneity, conceptual, 'Whether competing readings are simultaneous or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(lycu_tr_t0, projected).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__demographic_trap_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(lycu_tr_t50, observed).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__demographic_trap_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(lycu_tr_t100, observed).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__demographic_trap_reading, theater_ratio, 150, 0.48).
narrative_ontology:measurement_basis(lycu_tr_t150, observed).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement_basis(lycu_tr_t200, observed).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.68).
narrative_ontology:measurement_basis(lycu_tr_t300, observed).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__demographic_trap_reading, theater_ratio, 400, 0.68).
narrative_ontology:measurement_basis(lycu_tr_t400, observed).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(lycu_be_t0, projected).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__demographic_trap_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(lycu_be_t50, observed).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__demographic_trap_reading, base_extractiveness, 100, 0.74).
narrative_ontology:measurement_basis(lycu_be_t100, observed).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__demographic_trap_reading, base_extractiveness, 150, 0.81).
narrative_ontology:measurement_basis(lycu_be_t150, observed).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.85).
narrative_ontology:measurement_basis(lycu_be_t200, observed).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.89).
narrative_ontology:measurement_basis(lycu_be_t300, observed).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__demographic_trap_reading, base_extractiveness, 400, 0.89).
narrative_ontology:measurement_basis(lycu_be_t400, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(lycu_su_t0, projected).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__demographic_trap_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(lycu_su_t50, observed).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__demographic_trap_reading, suppression_requirement, 100, 0.76).
narrative_ontology:measurement_basis(lycu_su_t100, observed).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__demographic_trap_reading, suppression_requirement, 150, 0.82).
narrative_ontology:measurement_basis(lycu_su_t150, observed).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.87).
narrative_ontology:measurement_basis(lycu_su_t200, observed).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.92).
narrative_ontology:measurement_basis(lycu_su_t300, observed).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__demographic_trap_reading, suppression_requirement, 400, 0.92).
narrative_ontology:measurement_basis(lycu_su_t400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__demographic_trap_reading, 0.25).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, spartan_helot_subjection_system).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, identity_locked_citizenship_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Lycurgan_laws kernel. The sibling readings (sacral_fidelity_reading, adaptive_fiction_reading) offer alternative structural interpretations of the same legal code and enforcement apparatus, distinguished by their axioms about the nature of immutability, the historical outcome, and the mechanism of persistence. Decomposition reasoning: the ε value for this reading (0.89, snare classification) differs substantially from the sacral_fidelity reading's expected ε (~0.40, genuine coordination + sacred duty) because the two readings make different claims about what the law IS and what it DOES. Each reading has its own stakeholder experience, beneficiary structure, and temporal trajectory. They are linked via network.affects_constraints because the choice of reading (which interpretation of Lycurgus wins in the historical narrative) changes how the entire Spartan political system is classified. The demographic_trap reading upstream-influences the sacral_fidelity reading: evidence that demographic collapse occurred and that reform was suppressed creates empirical pressure on the sacral reading's axiom of divine ordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
