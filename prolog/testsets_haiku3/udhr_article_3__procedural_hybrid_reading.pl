% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: Article 3 Procedural Hybrid: Due Process Without Substantive Resolution
 *   domain: constitutional/human_rights
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights guarantees 'the
 *   right to life, liberty and security of person.' The procedural hybrid
 *   reading interprets this as guaranteeing due process protections — habeas
 *   corpus, torture prohibition, judicial review of detention — without
 *   resolving the deeper contest between negative liberty (freedom from state
 *   interference) and positive entitlement (state provision of material
 *   welfare). This reading is distinct from two sibling interpretations: the
 *   negative liberty reading, which emphasizes prohibition of state
 *   deprivation; and the positive entitlement reading, which obligates the
 *   state to provide material conditions for life and security. The
 *   procedural hybrid reading brackets the substantive dispute and focuses on
 *   *how* detention occurs: the procedure, not the reason or the material
 *   provision. It coexists with both sibling readings in international
 *   practice—courts, treaty bodies, and states navigate among all three
 *   simultaneously, deploying each as institutional context permits.
 *
 * KEY AGENTS:
 *   - detained_persons: subject to detention, beneficiary of procedural protections but not guaranteed material conditions or substantive liberty
 *   - judicial_oversight_institutions: gatekeepers of detention legality, institutional beneficiaries of the constraint
 *   - state_security_apparatus: must comply with procedural limits, bears cost of judicial review
 *   - negative_liberty_advocates: find the procedural reading congenial and strategically useful
 *   - positive_entitlement_advocates: view the reading as incomplete but not contradictory
 *   - treaty_monitoring_bodies: analytical observers interpreting the constraint over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.38).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "Article 3 Procedural Hybrid: Due Process Without Substantive Resolution").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '09f6a7c8-3648-47f8-ba05-ffede1c00255').
narrative_ontology:cs_kernel_codification('09f6a7c8-3648-47f8-ba05-ffede1c00255', formalized).
narrative_ontology:cs_authority_grounding('09f6a7c8-3648-47f8-ba05-ffede1c00255', lineage).
narrative_ontology:cs_interpretation_layer_present('09f6a7c8-3648-47f8-ba05-ffede1c00255').
narrative_ontology:cs_reading_relation('09f6a7c8-3648-47f8-ba05-ffede1c00255', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('09f6a7c8-3648-47f8-ba05-ffede1c00255', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('09f6a7c8-3648-47f8-ba05-ffede1c00255', foundational, detention_justiciability_required).
narrative_ontology:cs_axiom_status(detention_justiciability_required, holdable).
narrative_ontology:cs_axiom_grounding('09f6a7c8-3648-47f8-ba05-ffede1c00255', detention_justiciability_required, deontological).
narrative_ontology:cs_axiom('09f6a7c8-3648-47f8-ba05-ffede1c00255', foundational, torture_absolutely_prohibited).
narrative_ontology:cs_axiom_status(torture_absolutely_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('09f6a7c8-3648-47f8-ba05-ffede1c00255', torture_absolutely_prohibited, deontological).
narrative_ontology:cs_axiom('09f6a7c8-3648-47f8-ba05-ffede1c00255', secondary, procedural_sufficiency_for_liberty).
narrative_ontology:cs_axiom_status(procedural_sufficiency_for_liberty, holdable).
narrative_ontology:cs_axiom_grounding('09f6a7c8-3648-47f8-ba05-ffede1c00255', procedural_sufficiency_for_liberty, instrumental).
narrative_ontology:cs_reference_frame('09f6a7c8-3648-47f8-ba05-ffede1c00255', procedural_due_process_paradigm).
narrative_ontology:cs_drift_state('09f6a7c8-3648-47f8-ba05-ffede1c00255', contemporary_counter_terror_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09f6a7c8-3648-47f8-ba05-ffede1c00255', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detained_persons).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, judicial_oversight_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, negative_liberty_advocates).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, positive_entitlement_advocates).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, positive_entitlement_advocates).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, emergency_authorities).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, rule_of_law_proceduralism).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, judicial_independence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons subject to state detention gain explicit protections: habeas corpus access, torture prohibition, right to be informed of charges, judicial review of detention legality. These are procedural guarantees independent of whether the state must provide welfare or material conditions. The constraint guarantees the detained person can petition a court for release if detention violates the procedure, but does not guarantee material conditions during detention.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% Courts gain a mandated role as gatekeepers of detention legality. The constraint requires judicial review of detention, which institutionalizes the judiciary's power to check executive detention authority. Courts become structural beneficiaries by acquiring authority that the procedural hybrid reading reserves to them.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, judicial_oversight_institutions, beneficiary,
    institutional, generational, mobile, universal).

% Must operate detention within procedural limits: no torture, no indefinite detention without review, must allow habeas corpus access, must justify detention to a court. The constraint does not prohibit detention itself or require the state to provide welfare; it constrains the *method* of detention and requires periodic judicial justification. The apparatus pays the cost of judicial oversight and procedural compliance.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, state_security_apparatus, payer,
    institutional, generational, constrained, universal).

% Those who read Article 3 as prohibiting state deprivation except via narrow procedural justice find this reading congenial: it enshrines due process and torture prohibition. They can claim the constraint vindicates their position while remaining agnostic on welfare. This reading serves their interests without foreclosing them.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, negative_liberty_advocates, beneficiary,
    organized, generational, mobile, universal).

% Those who read Article 3 as obligating material welfare provision find this reading incomplete but compatible: it acknowledges procedural safeguards and judicial oversight, which can be leveraged to argue for material conditions as a *consequence* of the due process right (e.g., habeas corpus relief could require providing necessities). They view the constraint as under-enforced rather than contradictory, and it does not foreclose their advocacy.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, positive_entitlement_advocates, beneficiary,
    organized, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, positive_entitlement_advocates, payer).

% In declared emergencies, face pressure to balance detention authority against procedural constraints. The hybrid reading permits detention but requires judicial review even in emergencies, creating tension between speed and due process. Emergency authorities must justify detention to courts rather than retain unilateral authority.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, emergency_authorities, payer,
    institutional, biographical, constrained, universal).

% UN Human Rights Committee and regional bodies interpret Article 3 and assess compliance. They operate as analytical observers evaluating whether states honor the procedural guarantees. Their interpretations can shift the meaning of the procedural hybrid reading over time.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, treaty_monitoring_bodies, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared procedural framework for detention that is universally applicable: any state party must allow judicial review, prohibit torture, permit habeas corpus access. This solves the coordination problem of what detention legality looks like across different legal systems — instead of each state defining its own torture tolerance or habeas access, the constraint creates a floor. The coordination is *procedural*, not substantive: states retain discretion over *reasons* for detention (security, crime, emergency) but not over *methods* (no torture, no indefinite detention without review).
% TRANSFER_FUNCTION: Moves authority from the security apparatus to the judiciary: decisions about detention legality must be reviewed by independent courts rather than made unilaterally by executive police or military. It also transfers legitimacy from raw power (the apparatus can detain because it can) to law (the apparatus can detain only if a court permits it). The transfer is institutional rather than economic; it does not move money or material goods, but rather decision-making power.
% ABSENT_VOICES: Detained persons themselves have structurally limited voice in the reading's articulation — they are described as beneficiaries but they do not typically author the interpretation of Article 3. Those who would read Article 3 as obligating state provision of food, medicine, or shelter during detention are present in some treaty bodies but absent from the procedural hybrid reading's core formulation. States that view detention as a sovereign prerogative (not subject to external judicial review) are excluded from the consent framework; they ratify the treaty but contest its meaning.
% DISAPPEARANCE_RATIONALE: If the procedural hybrid reading vanished and states reverted to unilateral detention authority without judicial review, the consequences would include: indefinite detention without legal recourse, torture and abuse with no accountability mechanism, and the erosion of habeas corpus in law and practice. Detained persons would lose the formal right to petition courts; courts would lose their gatekeeping role; the security apparatus would regain unilateral authority. The world does not rearrange *in principle* (detention itself would persist) but does rearrange *institutionally*: courts lose power, detained persons lose legal remedy, and rule-of-law doctrine loses a core application.
% FOUNDING_PROBLEM: Historical detention practices showed that without procedural safeguards, states torture, hold persons indefinitely without charges, and deny legal remedy. The founding problem was the absence of due process: states could disappear persons, deny them access to counsel or courts, and abuse them with impunity. Article 3 was drafted to make detention *justiciable* — i.e., reviewable by an independent judge — rather than unilateral executive action.
% FOUNDING_PROBLEM_CORROBORATION: Amnesty International, Human Rights Watch, UN Special Rapporteur on Torture, and domestic courts across multiple jurisdictions document ongoing torture, secret detention, and denial of habeas corpus access in conflict zones, counter-terrorism operations, and authoritarian regimes. These external observers attest the founding problem persists. States claim the problem is solved by the existence of the treaty and their official policies, but the monitoring bodies report systematic violation. The founding problem remains live on the evidence of practitioners outside the benefiting parties (courts and treaty bodies).
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.38 at interval end) reflects moderate institutional cost: the constraint requires judicial review and forbids torture, but does not require the state to provide material welfare or narrow the grounds for detention. Early in the interval (t=0), extractiveness was lower (0.22) because the procedural reading was newly articulated and compliance machinery was thin; by t=50, it stabilized at 0.38 as treaty bodies developed consistent jurisprudence and states adapted their detention procedures. The theater_ratio rises from 0.18 to 0.41 over the interval, indicating growing performative compliance: states adopt habeas corpus procedures on paper while systematically violating them in practice (secret detention despite procedural requirements, torture despite prohibition, indefinite detention despite judicial review mandates). This theatrical pattern reflects the constraint's structural vulnerability: it is easy to enact the procedure and difficult to enforce it against a security apparatus determined to evade it. Suppression_requirement also rises from 0.35 to 0.52, tracking the enforcement burden: as states recognize the judicial review requirement, they invest in counter-strategies (special security courts, deference doctrines favoring the executive, emergency exceptions) that suppress the constraint's force. The measurement trajectory shows a constraint with real coordination function (due process is valued) but rising institutional capture (states preserve detention authority while formally complying).
 *
 * PERSPECTIVAL GAP:
 *   The detained person and the security apparatus experience radically different constraints. For the detained person, Article 3 as procedurally read offers a justiciability guarantee: they can petition a court, claim torture, demand habeas corpus. For the security apparatus, it imposes a cost: detention must be reviewable, torture is prohibited, indefinite detention is limited. From the judiciary's perspective, the constraint is an expansion of institutional authority: they become gatekeepers of detention legality. The negative liberty advocate and the positive entitlement advocate read the same text and extract different emphases: the negative liberty reading emphasizes the prohibition on state deprivation; the positive reading reads the right to security as implying state provision. The procedural hybrid reading avoids choosing between them by focusing on *process*: both readings can claim procedural due process is necessary. The engine should compute different types for these seats: the detained person (trapped, powerless) sits in a snare-adjacent position (constrained by procedure but without guarantee of freedom or welfare); the judiciary (institutional, mobile) sits in a rope position (genuine coordination role). The security apparatus (institutional, constrained by the requirement to justify) sits between tangled rope and snare (coordinating on procedure while extracting detention authority). The gap reflects structural asymmetry, not authoring mistake.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons benefit from procedural protections (low d, near beneficiary end) but remain trapped and powerless (exit constrained by physical detention). Their directionality is pulled toward the beneficiary end by the procedural guarantees but remains moderate because the guarantee does not provide material welfare, substantive liberty, or release. Judicial institutions benefit from expanded authority (low d, beneficiary end) and have mobile exit options (they can interpret the constraint expansively or narrowly). The security apparatus pays a cost (judicial review requirement, torture prohibition) but retains detention authority (constrained d, not fully target). Negative liberty advocates and positive entitlement advocates both benefit from having a procedural anchor, though they would extend it differently. Neither is a payer; both are secondary beneficiaries who can leverage the constraint toward their reading. The directionality derivation should show judicial institutions with the lowest d (institutional, mobile, clear beneficiary), followed by detained persons (powerless, trapped, constrained beneficiary), followed by the security apparatus (institutional, constrained by review requirement, near symmetric or slightly extractive). No directionality overrides are needed; the structural data suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The procedural hybrid reading avoids mandatrophy by keeping the constraint narrowly focused on process. It does not claim the founding problem (torture, indefinite detention, denial of habeas corpus) is solved; it only claims the solution is *procedural review*, not substantive material provision or absolute prohibition on detention. This reading is mandatrophy-resistant because it can update its founding problem status as needed: if detention procedures are followed but torture persists (secret detention with judicial review blessing), the reading can attribute this to enforcement failure rather than conceptual obsolescence. The constraint remains justified as long as detention *can be reviewed*, regardless of whether review always succeeds. However, the rising theater_ratio (0.41 at interval end) suggests incipient mandatrophy: if procedural review becomes purely performative (courts rubber-stamp detention, torture is classified as permitted interrogation), the founding problem remains live but the constraint's function atrophies. At high theater ratios, the procedural hybrid reading risks becoming a piton: the procedure is maintained as theater while substantive detention authority remains uncontrolled. The commentary acknowledges this risk and an omega variable addresses it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_substantive_boundary,
    'Can Article 3''s procedural protections be sustained as a reading if the substantive liberty/welfare dispute remains unresolved indefinitely? Or does the procedural reading implicitly favor one side of the substantive contest?',
    'Longitudinal analysis of treaty body jurisprudence: if courts increasingly issue substantive holdings on material conditions (food, medicine, shelter) while invoking Article 3, the procedural reading will have been assimilated into substantive interpretation; if courts consistently defer on substantive issues and ground decisions only in procedure, the reading remains stable.',
    'If the procedural reading is unsustainable (substantive issues inevitably force themselves into any application), the reading becomes transitional rather than equilibrial, and the constraint reclassifies toward the winning substantive reading. If sustainable, the reading represents a genuine structural possibility and no reclassification occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_boundary, conceptual, 'Whether the procedural/substantive distinction can be maintained as a stable boundary or whether substantive issues will penetrate the procedural framing.').

omega_variable(
    emergency_detention_limits,
    'In declared emergencies (war, terrorism, pandemic), do procedural protections persist with their original force, or does the emergency exception erode the habeas corpus and torture prohibition guarantees?',
    'Comparative case law analysis: measure the rate at which courts deny habeas corpus in emergency contexts, permit ''enhanced interrogation,'' or defer to executive detention claims. If the rate is substantially higher in emergencies, the procedural reading breaks down in precisely the moments it is most needed.',
    'If emergencies systematically exempt detention from judicial review or torture prohibition, the constraint is not universally applicable and should be reclassified as a rope (coordination that holds in normal times) rather than an unconditional due process guarantee. The extractiveness and suppression scores should rise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_detention_limits, empirical, 'Whether procedural protections hold in emergencies or whether they systematically erode under pressure.').

omega_variable(
    sibling_reading_incompatibility,
    'Can a court or state simultaneously apply the procedural hybrid reading while also endorsing the negative liberty reading (narrow grounds for detention) or the positive entitlement reading (material provision as a right)? Or are the readings mutually exclusive in practice?',
    'Analysis of adjudications in jurisdictions that invoke Article 3: do courts cite to substantive liberty or welfare holdings in the same decision where they ground decisions in procedure alone? If yes, coexistence is maintained; if no, the readings are practically exclusive.',
    'If coexistence is maintained, the kernel remains contested and all three readings remain live. If the readings prove practically exclusive (a court must choose one), the constraint reclassifies as representing a foreclosure relationship rather than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_incompatibility, empirical, 'Whether the sibling readings can coexist in actual adjudication or whether courts are forced to choose.').

omega_variable(
    theater_ratio_threshold,
    'At what point does procedural compliance become purely theatrical (detention procedures exist on paper but are systematically violated in practice) rather than substantively effective?',
    'Post-exit accountability: do states that formally adopt habeas corpus and torture prohibition show measurably lower rates of disappearance, torture, and indefinite detention than states without formal procedures? If not, the procedures are theater.',
    'If theater_ratio approaches 0.7+, the constraint reclassifies from rope toward piton (atrophied function maintained as institutional theater). At this threshold, the procedural hybrid reading becomes inoperative and the constraint''s persistence depends on momentum rather than function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_threshold, empirical, 'Whether procedural requirements translate into measurable reductions in detention abuse or remain performative.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the rising suppression_requirement (0.52 at interval end) driven by external barriers (courts refusing to grant habeas corpus, executive resistance to judicial review) or by internalization (detainees have internalized the belief that courts will not help them, so they stop petitioning)?',
    'Data on habeas corpus filing rates and success rates over time: if filings decline while courts remain willing to grant relief, suppression is internalized; if filings hold steady but courts deny relief, suppression is structural (external barriers).',
    'If internalized, the constraint''s suppression persists even after formal abolition (detainees carry the suppression belief after release), suggesting deep institutional capture. If structural, removing the barrier (empowering courts) restores the constraint''s function. The classification difference affects remedies: internalized suppression requires re-legitimation of courts; structural suppression requires removing barrier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of the habeas corpus guarantee is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(udhr_tr_t0, observed).
narrative_ontology:measurement(udhr_tr_t12, udhr_article_3__procedural_hybrid_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(udhr_tr_t12, observed).
narrative_ontology:measurement(udhr_tr_t25, udhr_article_3__procedural_hybrid_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement_basis(udhr_tr_t25, observed).
narrative_ontology:measurement(udhr_tr_t38, udhr_article_3__procedural_hybrid_reading, theater_ratio, 38, 0.4).
narrative_ontology:measurement_basis(udhr_tr_t38, observed).
narrative_ontology:measurement(udhr_tr_t50, udhr_article_3__procedural_hybrid_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(udhr_tr_t50, observed).
narrative_ontology:measurement(udhr_tr_t62, udhr_article_3__procedural_hybrid_reading, theater_ratio, 62, 0.41).
narrative_ontology:measurement_basis(udhr_tr_t62, observed).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__procedural_hybrid_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(udhr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(udhr_be_t0, observed).
narrative_ontology:measurement(udhr_be_t12, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement_basis(udhr_be_t12, observed).
narrative_ontology:measurement(udhr_be_t25, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement_basis(udhr_be_t25, observed).
narrative_ontology:measurement(udhr_be_t38, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 38, 0.37).
narrative_ontology:measurement_basis(udhr_be_t38, observed).
narrative_ontology:measurement(udhr_be_t50, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(udhr_be_t50, observed).
narrative_ontology:measurement(udhr_be_t62, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 62, 0.38).
narrative_ontology:measurement_basis(udhr_be_t62, observed).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 75, 0.38).
narrative_ontology:measurement_basis(udhr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(udhr_su_t0, observed).
narrative_ontology:measurement(udhr_su_t12, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(udhr_su_t12, observed).
narrative_ontology:measurement(udhr_su_t25, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(udhr_su_t25, observed).
narrative_ontology:measurement(udhr_su_t38, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 38, 0.53).
narrative_ontology:measurement_basis(udhr_su_t38, observed).
narrative_ontology:measurement(udhr_su_t50, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement_basis(udhr_su_t50, observed).
narrative_ontology:measurement(udhr_su_t62, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 62, 0.52).
narrative_ontology:measurement_basis(udhr_su_t62, observed).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement_basis(udhr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% The UDHR Article 3 kernel is decomposed into three structurally distinct constraint stories. The procedural_hybrid_reading (this story) focuses on due process and judicial review without resolving the substantive liberty/welfare dispute. The negative_liberty_reading emphasizes the prohibition on state deprivation and interprets security as freedom from state violence. The positive_entitlement_reading interprets Article 3 as obligating material welfare provision. These are not alternative measurements of one constraint but three readings of a contested kernel, with different ε values, different beneficiary structures, and different mandates. Each story is linked via network.affects_constraints to its siblings. The procedural reading coexists with both substantive readings in international practice but creates structural pressure on both: by institutionalizing procedural review, it creates a venue for substantive claims (courts can order material provision as a consequence of release), and by remaining agnostic on substantive grounds, it permits both negative liberty and positive entitlement advocates to claim vindication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
