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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Article 3 Due Process Protections (Procedural Hybrid Reading)
 *   domain: constitutional/human_rights
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights states: 'Everyone
 *   has the right to life, liberty and security of person.' The procedural
 *   hybrid reading interprets this as guaranteeing due process
 *   protections—habeas corpus, torture prohibition, judicial review of
 *   detention, right to speedy trial—without mandating any substantive
 *   resolution of the contest between negative liberty (freedom from state
 *   interference) and positive entitlements (state obligation to provide
 *   welfare/healthcare/housing necessary for life). The reading is
 *   fundamentally about procedure: how states must act when they detain, not
 *   whether they may detain at all, and not what material conditions the
 *   state must provide. This sits between the negative liberty reading
 *   (Article 3 = constraint on state power) and the positive entitlement
 *   reading (Article 3 = mandate for state provision). The procedural hybrid
 *   reading strategically defers the substantive contest, making it the
 *   consensus basis for international human rights enforcement.
 *
 * KEY AGENTS:
 *   - Detained persons (beneficiaries of habeas and torture prohibition; trapped in the constraint; immediate time horizon)
 *   - Judicial/international review bodies (agenda-setters; institutional power; generational time horizon; set the meaning of 'torture' and 'due process')
 *   - State security apparatus (payers of the constraint; constrained exit; lose discretion in detention and interrogation)
 *   - Emergency-powers claimants (institutional payers; resist the constraint during crises; argue procedure impedes security)
 *   - Welfare-provision states (excluded by the reading's design; would argue Article 3 mandates positive provision)
 *   - International human rights bodies (institutional agenda-setters; enforce procedural norms across jurisdictions)
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
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "Article 3 Due Process Protections (Procedural Hybrid Reading)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, 'dd90377e-d460-4bd2-8126-aace8a4058a5').
narrative_ontology:cs_kernel_codification('dd90377e-d460-4bd2-8126-aace8a4058a5', fixed_text).
narrative_ontology:cs_authority_grounding('dd90377e-d460-4bd2-8126-aace8a4058a5', lineage).
narrative_ontology:cs_interpretation_layer_present('dd90377e-d460-4bd2-8126-aace8a4058a5').
narrative_ontology:cs_reading_relation('dd90377e-d460-4bd2-8126-aace8a4058a5', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd90377e-d460-4bd2-8126-aace8a4058a5', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('dd90377e-d460-4bd2-8126-aace8a4058a5', foundational, procedure_sufficiency_without_substance_resolution).
narrative_ontology:cs_axiom_status(procedure_sufficiency_without_substance_resolution, holdable).
narrative_ontology:cs_axiom_grounding('dd90377e-d460-4bd2-8126-aace8a4058a5', procedure_sufficiency_without_substance_resolution, deontological).
narrative_ontology:cs_axiom('dd90377e-d460-4bd2-8126-aace8a4058a5', foundational, torture_prohibition_absoluteness).
narrative_ontology:cs_axiom_status(torture_prohibition_absoluteness, holdable).
narrative_ontology:cs_axiom_grounding('dd90377e-d460-4bd2-8126-aace8a4058a5', torture_prohibition_absoluteness, deontological).
narrative_ontology:cs_axiom('dd90377e-d460-4bd2-8126-aace8a4058a5', secondary, habeas_corpus_supremacy).
narrative_ontology:cs_axiom_status(habeas_corpus_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('dd90377e-d460-4bd2-8126-aace8a4058a5', habeas_corpus_supremacy, instrumental).
narrative_ontology:cs_reference_frame('dd90377e-d460-4bd2-8126-aace8a4058a5', procedurally_just_detention_and_interrogation).
narrative_ontology:cs_drift_state('dd90377e-d460-4bd2-8126-aace8a4058a5', contemporary_counter_terrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dd90377e-d460-4bd2-8126-aace8a4058a5', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detained_persons).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, judicial_review_system).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, emergency_powers_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, torture_survivors_and_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty by state action—prisoners, suspects, persons held during war or emergency. Article 3's procedural protections (habeas corpus, torture prohibition, judicial review, right to speedy trial) create remedies against arbitrary detention and coercive interrogation. They cannot exit the constraint; it applies regardless of their preference. They benefit from the procedures themselves and from the authority transfer from police to courts. They do not benefit (under this reading) from any state obligation to provide welfare; that is the positive entitlement reading's domain. They are the reading's primary beneficiaries.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% Courts, tribunals, human rights committees, and judicial institutions that enforce Article 3. They gain authority, docket, and soft power from habeas jurisdiction. They set the formal meaning of 'torture,' 'prompt trial,' 'inhuman treatment.' They coordinate consistency across jurisdictions via case law and treaty interpretation. A court that narrows procedural protections loses jurisdiction and international standing; one that expands them gains both. They are institutional agenda-setters because they operationalize the constraint through rulings.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, judicial_review_system, beneficiary,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, judicial_review_system, agenda_setter).

% Police, military, intelligence agencies, detention facilities, and interrogation operations. Article 3 constrains their freedom: detention must be documented and reported, suspects must be charged or released within a time limit, torture is absolutely prohibited, courts can order release on habeas review. These are operational costs—slower processing, reduced interrogation options, loss of discretion, court reversals. They resist the constraint structurally because it directly limits what they do. Their argument is not ideological opposition to due process in principle, but that procedure impedes effective security operations. They are constrained but not trapped; they can argue emergency exceptions and seek political support for narrowing procedure, but they cannot simply ignore court orders.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, state_security_apparatus, payer,
    institutional, generational, constrained, universal).

% Executive officials, military commanders, and counter-terrorism authorities who invoke national security, terrorism threats, or war to justify suspending or narrowing procedural protections. Article 3's torture prohibition and habeas requirement constrain emergency detention policies; they must justify exceptions in court. The procedural hybrid reading does not resolve whether emergencies permit suspension (that is the contested boundary). They are payers because the constraint limits their emergency powers; they are constrained because they can argue necessity, but they must do so legally, not unilaterally.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, emergency_powers_claimants, payer,
    institutional, biographical, constrained, universal).

% States and advocacy coalitions that read Article 3 as mandating positive state provision of healthcare, housing, food, and other material conditions as necessary for 'life and security.' They are excluded from the procedural hybrid reading's immediate scope; the reading does not address substantive entitlements. They challenge the reading as incomplete and argue that procedure without provision is empty. They have organizational capacity (can lobby, litigate, politicize) and are mobile (can shift resources to other strategies or jurisdictions), so they are not trapped by the reading. They are the primary pressure point for the reading's destabilization.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, welfare_provision_states, excluded,
    institutional, generational, mobile, regional).

% Persons subjected to torture, survivors' movements, and human rights organizations dedicated to torture prevention and accountability. They benefit from the absolute prohibition on torture and from judicial remedy availability. They also bear a cost: the procedural hybrid reading does not mandate investigation, prosecution, or reparation as substantive entitlements within its frame. The reading protects them from future torture but may not fully vindicate past harms. They are observers because they have a stake but are not parties to the state-detention relationship; they are also beneficiaries because the torture prohibition directly protects them.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, torture_survivors_and_advocates, observer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, torture_survivors_and_advocates, beneficiary).

% UN human rights bodies, regional courts (European Court of Human Rights, Inter-American Court, African Commission), treaty monitoring committees, and international NGOs. They operationalize Article 3 through case law, treaty interpretation, and monitoring reports. They set the formal meaning of due process globally and coordinate consistency across jurisdictions. They have institutional interest in robust procedural enforcement, which the hybrid reading supports. They are agenda-setters because they interpret the constraint for multiple constituencies and have authority to influence state policy.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, international_human_rights_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% States that prioritize security over procedure and resist international human rights enforcement. They argue that Article 3 should be suspended or radically narrowed during emergencies, that courts should defer to executive judgments on necessity, and that international bodies should not second-guess national security decisions. The procedural hybrid reading constrains them via international oversight and judicial review; they experience this as external imposition. They are excluded by the constraint's design because they would reject its premises. They are trapped at the international level (cannot exit UDHR or international treaty commitments) but mobile at the national level (can attempt withdrawal or non-compliance). They are the primary structural resistance.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, nationalist_and_security_first_states, excluded,
    institutional, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, enforceable floor for procedural justice in detention and interrogation: habeas corpus (right to challenge detention lawfully), absolute prohibition on torture, right to speedy trial, and judicial review of state detention decisions. These procedures solve the coordination problem of preventing arbitrary state violence without requiring all parties to agree on what liberty or security ultimately means. Different states can hold different substantive views (whether Article 3 means freedom from state interference or state obligation to provide welfare) while sharing procedural commitments. The coordination is the procedure itself, not the underlying substantive question.
% TRANSFER_FUNCTION: Transfers authority over detention and interrogation from state officials (police, military, executive) to judicial institutions. Transfers the burden of justification from detainees (to prove why they should be released) to the state (to prove lawful grounds, proportionality, necessity). Transfers the power to define 'torture' and 'humane treatment' from operational agencies to courts and treaty bodies. Transfers the power to decide detention timeline from executive to judicial review. This authority reallocation is the constraint's primary flow—not money or goods, but decision-making power and veto authority. Courts gain authority; security agencies lose discretion.
% ABSENT_VOICES: States claiming absolute emergency powers and freedom from judicial review are structurally excluded by the constraint's design; they would argue that procedure is a luxury in wartime and that courts cannot second-guess military necessity. Persons in clandestine or disappeared detention are absent by definition—the habeas right assumes a system where detentions are reported and documented, which breaks down when detention is hidden. Welfare-maximizing states and positive-entitlement advocates are excluded from the substantive question the hybrid reading deliberately leaves open; they are present in advocacy but absent from the constraint's operational scope.
% DISAPPEARANCE_RATIONALE: If Article 3's procedural protections vanished, state detention would become arbitrary and coercive without legal restraint; courts would lose jurisdiction to review detention decisions; torture would become a permissible interrogation method without legal consequence; detainees would have no remedy and no right to challenge their confinement. The world would reorganize around unrestricted state coercive power over imprisonment. The institutional balance between state power and legal constraint would fundamentally shift toward state power. International human rights bodies would lose their primary enforcement mechanism. The constraint's removal would unmake the procedural justice system it sustains.
% FOUNDING_PROBLEM: Early-20th-century totalitarian regimes (Nazi Germany, Stalinist USSR, Imperial Japan) demonstrated that torture, arbitrary detention, secret detention, and extrajudicial execution could destroy human dignity and political trust without legal check. During and after World War II, systematic documentation of these crimes created a global consensus that detention must be lawful, documented, time-limited, and subject to judicial review. Article 3 was drafted to solve the problem of unconstrained state violence: detention without trial, interrogation via torture, disappearances, and summary execution. The founding problem is not whether states may detain suspects (a substantive question about security and crime control), but how they must do so—with what procedures, oversight, and safeguards.
% FOUNDING_PROBLEM_CORROBORATION: Independent documentation from multiple contemporary human rights organizations (Amnesty International, Human Rights Watch, UN Office of the High Commissioner for Human Rights, Physicians for Human Rights) confirms that torture and secret detention persist globally despite Article 3 and its treaty elaborations. National court cases and international tribunal decisions cite the founding problem as justification for enforcing habeas, investigating torture, and requiring disclosure of detention locations. Victims' testimony and forensic evidence from contemporary cases (post-9/11 detention facilities, Syrian detention, Myanmar military detention) demonstrate the ongoing reality of the problem Article 3 addresses. The problem is not confined to historical totalitarian regimes; it is actively renewed in contemporary counter-terrorism operations, military conflicts, and emergency detention. Corroboration comes from observers outside the human rights beneficiary coalition: journalists, researchers, and governments with no institutional stake in the constraint's enforcement. The founding problem remains structurally live.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   The procedural hybrid reading produces moderate extractiveness (0.38) because it imposes real costs on state security agencies—detention must be documented, torture is prohibited, courts can review—without resolving the underlying substantive contest. This is a genuine tangled rope: the coordination is real (all parties commit to procedural justice), but the extraction is also real (security agencies lose operational discretion). Suppression is moderate (0.52) because the constraint's enforcement depends on court capacity and state compliance; it is actively resisted by security actors and emergency-power claimants but not uniformly suppressed by coercion (courts are public and visible). Theater is moderate (0.41) because compliance theatricality is observable: some states detain secretly, some torture while officially denying it, some perform judicial review while gutting habeas remedies. The measurement series show extractiveness rising slightly from t=0 to t=15 as security threats increase global detention volumes, then slightly declining as international pressure and jurisprudence strengthen procedural enforcement. Suppression follows a similar arc: peaking at t=15–20 (height of emergency detention expansion) then declining as litigation forces procedural compliance. Theater rises throughout because states increasingly must perform compliance (due process hearings, torture investigations) even where the substance erodes.
 *
 * PERSPECTIVAL GAP:
 *   The judicial/review seat and the detained-person seat compute the constraint as protective coordination (rope-adjacent); the state security seat computes it as imposed extraction (snare-adjacent). From the court's position, Article 3 procedures are the framework for legitimate detention and interrogation—coordination that actually enables targeted security work. From the security apparatus's position, the same procedures are obstacles: they slow operations, create publicity, enable defense lawyers to obstruct interrogation, and generate liability. From the detainee's position, the procedures are purely beneficial—the sole mechanism for challenging arbitrary state action. The engine's per-seat computation should show this divergence: the institutional beneficiaries (courts, detained persons) computing procedural_hybrid as rope; the institutional payers (security apparatus) computing it as snare-approaching due to constrained exit and active enforcement. The divergence is structural, not perceptual noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons are beneficiaries (d near 0.0): they gain remedy and protection without cost; they are trapped (cannot exit); low power. Judicial bodies are beneficiaries (d near 0.1–0.2): they gain authority and docket; they are arbitrage-positioned (can reinterpret the constraint); institutional power. State security apparatus are victims/payers (d near 0.8–0.9): they bear the operational cost of procedure; they are constrained but not trapped (can argue for emergency exceptions); institutional power. Emergency-power claimants are payers (d near 0.75–0.85): they lose discretion; constrained exit (can claim necessity but must justify it in court). The directionality for the security apparatus is high despite institutional power because the constraint directly targets their operational domain and they have no off-ramp except political argument (constrained exit, not arbitrage). Welfare-provision states are excluded (not seated; d is not defined for them). The overall asymmetry—beneficiaries gaining procedure without material cost, payers losing discretion without any substantive security commitment—is what makes this a tangled rope: genuine coordination (everyone needs stable rules for detention) layered over asymmetric extraction (security loses, courts gain).
 *
 * MANDATROPHY ANALYSIS:
 *   Article 3's founding problem (totalitarian torture and secret detention) remains live in contemporary counter-terrorism operations, as corroborated by independent human rights documentation. However, the procedural hybrid reading itself is increasingly strained: the substantive contest between negative liberty and positive entitlements has not been resolved by deferral; it has metastasized. Courts now face pressure to read welfare provisions into Article 3 (positive reading) or to narrow habeas during emergencies (negative reading by another route). The reading's survival depends on maintaining the procedural focus and avoiding substantive resolution—a difficult task as global inequality and migration pressures mount. Mandatrophy has not yet occurred (the constraint is still active and enforced), but the reading is exhibiting signs of strain: theater_ratio rising (performance of compliance without substance), extractiveness plateauing (courts losing capacity to enforce as detention volumes rise), and suppression declining (states finding ways to work around procedure). The theater rise is the earliest warning of potential degradation into a piton reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_derogation_boundary,
    'Does Article 3''s prohibition on torture and its habeas requirement admit emergency exceptions, and if so, what threshold justifies suspension of procedure?',
    'Case law from multiple jurisdictions (European Court of Human Rights, International Court of Justice, US Supreme Court) establishing whether emergency declarations can narrow habeas or modify torture standards, and what ''necessity'' must demonstrate.',
    'If the boundary permits substantial emergency narrowing, the constraint''s extraction (security cost) decreases and suppleness increases; if emergency exceptions are foreclosed, the constraint remains robust but faces more sustained resistance from security actors. Classification could shift from tangled_rope toward snare (if exceptions are permitted and widely used) or toward rope (if they are rejected).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_derogation_boundary, empirical, 'Whether the procedural hybrid reading permits emergency derogation from torture prohibition and habeas.').

omega_variable(
    positive_entitlement_creep,
    'As courts interpret Article 3''s ''right to life,'' will they increasingly read it as mandating positive state provision (healthcare, housing, food) rather than restricting the scope to procedural protection?',
    'Tracking global jurisprudence on Article 3 over a generational time horizon; monitoring whether courts incorporate socioeconomic rights into the constraint''s scope; observing whether positive-entitlement and procedural readings remain stable or merge.',
    'If courts converge on reading Article 3 as mandating both procedure AND provision, the procedural hybrid reading loses distinctiveness and merges into the positive-entitlement reading; extractiveness may rise if states resist welfare mandates more fiercely than procedure mandates; the constraint may bifurcate into separate procedural and welfare constraints. If readings remain separate, the procedural hybrid reading persists as a stable middle position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positive_entitlement_creep, conceptual, 'Whether the substantive contest between negative liberty and positive entitlements will eventually collapse the procedural hybrid reading into one of its siblings.').

omega_variable(
    theater_ratio_degradation_trajectory,
    'Is the rising theater_ratio (states performing compliance without substance) a sign of incipient piton-ization, or a phase in maturation of enforcement capacity?',
    'Post-performance analysis: audit whether states that perform judicial review actually comply with court orders, whether torture prohibitions are enforced with prosecution or only with rhetorical commitment, whether habeas availability corresponds to actual release rates. A divergence between performed and actual compliance indicates piton trajectory; convergence indicates maturation.',
    'If theater indicates piton degradation, the constraint is moving toward inertial maintenance without real function; extractiveness and suppression will eventually diverge from the procedure''s nominal scope. If theater indicates enforcement capacity growth, it is a normal phase; the constraint may stabilize at higher theater with real compliance underneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_degradation_trajectory, empirical, 'Whether rising theater_ratio signals incipient constraint degradation or enforcement-capacity maturation.').

omega_variable(
    torture_definition_instability,
    'What constitutes ''torture'' under Article 3, and is the definition being narrowed by state practice and judicial acquiescence over time?',
    'Comparative jurisprudence and interrogation practice documentation: monitor whether the set of prohibited practices expands (more techniques classified as torture) or contracts (previously prohibited techniques reclassified as coercion/duress and permitted). State practice during emergencies is the primary driver.',
    'If the torture definition contracts, the constraint''s protective scope erodes even as its formal structure persists—the hallmark of piton degradation. If the definition expands or holds, the constraint remains robust. This is also the mechanism by which positive-entitlement and negative-liberty readings can both influence the hybrid reading without explicitly merging with it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(torture_definition_instability, empirical, 'Whether the torture prohibition''s material scope is contracting through reinterpretation despite formal permanence.').

omega_variable(
    procedural_adequacy_without_substantive_resolution,
    'Can the procedural hybrid reading sustain legitimacy indefinitely while deferring resolution of whether Article 3 mandates positive material conditions, or will the deferent collapse?',
    'Monitor legitimacy of the reading via: (a) judicial adoption and stability (do courts consistently apply the hybrid reading?), (b) state compliance (do both negative-liberty and positive-entitlement states accept the procedural commitment?), (c) victim satisfaction (do detainees and torture survivors accept procedure as sufficient?), (d) pressure for renegotiation (do advocacy movements force the substantive question into the foreground?).',
    'If the deferential reading is genuinely stable, the procedural hybrid remains distinct and classified as tangled_rope indefinitely. If legitimacy erodes, the reading may be forced to take a side (becomes either negative or positive) or may collapse into a snare (procedure seen as insufficient protection layered on top of unresolved substantive vulnerability). Bifurcation is also possible: Article 3 splits into a procedural constraint and a separate positive-entitlement constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_adequacy_without_substantive_resolution, conceptual, 'Whether the reading''s strategic deferral of substantive contest is sustainable or inherently temporary.').

omega_variable(
    kernel_reading_contest_framing,
    'Is the procedural hybrid reading genuinely distinct from its siblings, or does it represent a strategic coalition position masking unresolved preference for one underlying reading?',
    'Genealogical analysis: trace which coalitions (courts, states, advocacy movements) support the procedural hybrid reading, and whether they simultaneously advance either the negative-liberty or positive-entitlement reading in other contexts. If the same actors advance all three simultaneously, the hybrid reading is a coalition position; if distinct actor coalitions support distinct readings, the hybrid reading is genuinely distinct.',
    'If the reading is a coalition position, it is vulnerable to coalition breakdown; the constraint''s classification and persistence depend on political stability rather than structural legitimacy. If it is genuinely distinct, it is more durable. This omega addresses the conceptual boundary between reading-independence and reading-instrumentality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Whether the procedural hybrid reading is structurally independent or a strategic political coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(udhr_tr_t5, udhr_article_3__procedural_hybrid_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement(udhr_tr_t10, udhr_article_3__procedural_hybrid_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__procedural_hybrid_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(udhr_tr_t20, udhr_article_3__procedural_hybrid_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(udhr_tr_t25, udhr_article_3__procedural_hybrid_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__procedural_hybrid_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(udhr_tr_t35, udhr_article_3__procedural_hybrid_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(udhr_be_t5, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(udhr_be_t10, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(udhr_be_t20, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(udhr_be_t25, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(udhr_be_t35, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 35, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(udhr_su_t5, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(udhr_su_t10, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(udhr_su_t20, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(udhr_su_t25, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(udhr_su_t35, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% Article 3 of the UDHR instantiates three structurally distinct constraints under the same textual kernel. The negative_liberty_reading emphasizes prohibition on state violence; the positive_entitlement_reading emphasizes state obligation to provide material conditions; the procedural_hybrid_reading (this story) emphasizes due process protections without resolving the substantive liberty/welfare contest. Each reading has its own ε (ranging from low for negative liberty to moderate for hybrid to higher for positive entitlement), beneficiary/victim structure, and operational challenges. All three readings cite Article 3 but describe different constraints. The three stories are linked by network.affects_constraints to enable joint analysis of how a single kernel distributes across multiple readings and how readings influence each other (the positive_entitlement reading's ascendance may raise pressure on the hybrid reading to take a stronger substantive stance; the negative_liberty reading's dominance in liberal jurisdictions may limit the hybrid reading's scope). The ε values are not averaged or compromised; each reading is authored as a complete constraint with its own classification and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
