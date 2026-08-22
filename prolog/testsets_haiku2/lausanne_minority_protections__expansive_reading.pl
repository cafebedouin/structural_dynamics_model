% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections (Expansive Reading): Pre-1923 Governance Continuity
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Treaty of Lausanne (1923) includes minority protections that the
 *   expansive reading interprets as guaranteeing functional continuity of
 *   pre-1923 religious governance, including institutional
 *   self-administration, property rights, and theological education. This
 *   constraint story instantiates that reading—one interpretive stance within
 *   a contested kernel. The expansive reading treats Lausanne protections as
 *   international commitments that override domestic Turkish law where they
 *   conflict, creating an international regime that secures minority
 *   institutional autonomy. This is NOT the only reading of the same treaty
 *   text; the restrictive reading confines protections to individual worship;
 *   the guarantor reading reframes them as conditional on international
 *   supervision. Each reading instantiates a different constraint with
 *   different beneficiary structures and extraction profiles. This story
 *   models the expansive reading as a genuine coordination mechanism—solving
 *   the problem of institutional continuity across state transition—with
 *   moderate enforcement costs and institutional vulnerability to
 *   interpretive loss.
 *
 * KEY AGENTS:
 *   - Ecumenical Patriarchate: beneficiary institutional actor trapped in Istanbul, dependent on treaty guarantee for ecclesiastical self-governance and property rights continuity
 *   - Armenian Apostolic Church: beneficiary institutional actor similarly positioned, reliant on treaty reading for institutional autonomy
 *   - Jewish communities: beneficiary actors with smaller institutional footprint and more mobility than Christian minorities
 *   - Turkish government: agenda-setter, interpreter of the treaty through domestic implementation, constrained by expansive reading but capable of shifting to restrictive interpretation
 *   - European human rights mechanisms: observer seats providing external verification and pressure but lacking direct enforcement
 *   - Guarantor states (Britain, France, successors): weakened observer positions in the post-WWII order
 *   - Turkish nationalist constituencies: excluded from the treaty protections framework, actively contesting the expansive reading in domestic politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.42).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections (Expansive Reading): Pre-1923 Governance Continuity").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, 'b0cb800c-e2fb-46f2-81c0-828c71fa1cd6').
narrative_ontology:cs_kernel_codification('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', fixed_text).
narrative_ontology:cs_authority_grounding('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', lineage).
narrative_ontology:cs_interpretation_layer_present('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6').
narrative_ontology:cs_reading_relation('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', foundational, institutional_autonomy_guarantee).
narrative_ontology:cs_axiom_status(institutional_autonomy_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', institutional_autonomy_guarantee, conventional).
narrative_ontology:cs_axiom('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', foundational, pre_1923_continuity_principle).
narrative_ontology:cs_axiom_status(pre_1923_continuity_principle, holdable).
narrative_ontology:cs_axiom_grounding('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', pre_1923_continuity_principle, conventional).
narrative_ontology:cs_reference_frame('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', ottoman_institutional_continuity_commitment).
narrative_ontology:cs_drift_state('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', contemporary_restrictive_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0cb800c-e2fb-46f2-81c0-828c71fa1cd6', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, orthodox_ecumenical_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_apostolic_church).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Patriarchate operates as a largely self-governing ecclesiastical institution under the expansive reading, maintaining its own theological seminary historically, selecting its own leadership without external veto, and managing substantial property holdings accumulated before 1923. The reading guarantees these functions persist as they existed in the pre-treaty era. Its exit is structurally impossible—the institution is rooted to Istanbul geographically and canonically; leaving would forfeit the centuries-old seat of ecumenical authority. The Patriarchate is numerically weakened and demographically vulnerable but institutionally committed to defending the treaty reading against Turkish state pressure and the competing restrictive interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, orthodox_ecumenical_patriarchate, beneficiary,
    institutional, civilizational, trapped, regional).

% Similarly positioned as the Patriarchate under this reading: self-administration of internal affairs, property protection, and clergy formation through its own ecclesiastical schools. Like the Patriarchate, the Armenian Church is institutionally rooted; exit from Turkish territory would mean abandoning the historic Armenian Quarter and centuries of institutional presence. The Church faces similar pressures as the Patriarchate but with an additional layer of ethnic-nationalist contestation in Turkish domestic politics.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_apostolic_church, beneficiary,
    institutional, civilizational, trapped, regional).

% Receive protections for internal governance of community affairs, religious education, and property stewardship under the expansive reading. Smaller and more dispersed than the Christian minorities, they depend on the same treaty reading for institutional continuity but have greater geographical mobility than the Patriarchate. Jewish communities have experienced significant demographic decline and emigration over the treaty period, complicating the question of whether institutional autonomy remains functionally necessary for their survival.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, jewish_communities, beneficiary,
    moderate, generational, constrained, local).

% Administers and interprets the Lausanne Treaty through domestic legislation and enforcement. Under this expansive reading, the government is bound to recognize institutional self-governance, property rights, and theological education for the named minorities—a constraint on its sovereignty. The government could shift to a restrictive interpretation or invoke the competing guarantor reading to reframe obligations as conditional and state-supervised rather than absolute. The government's position has shifted over time, with periods of relative accommodation (1920s–1960s) and periods of restrictive pressure (Halki closure 1971, property restrictions).
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_government, agenda_setter,
    institutional, generational, mobile, national).

% Monitor Turkish implementation through ECHR and treaty-body oversight. They provide external verification through case law and periodic monitoring reports but lack direct enforcement power; their role is clarification of treaty intent, naming of breaches, and articulation of standards that can trigger diplomatic pressure and EU conditionality. European mechanisms consistently interpret Lausanne in the expansive direction, reinforcing the expansive reading against Turkish restrictive moves.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_human_rights_mechanisms, observer,
    institutional, generational, analytical, continental).

% Britain and France originally designated as guarantors in the Treaty; successors in the post-WWII order exercise weaker direct oversight. They are named as guarantors in the Treaty but their enforcement capacity has attenuated significantly. Their role in this expansive reading is clarification of treaty intent and diplomatic intervention—less powerful than active supervision, more powerful than neutral observation. Their historical interventions have been episodic rather than sustained.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    institutional, generational, analytical, global).

% View minority institutional autonomy as incompatible with Turkish state sovereignty and national integration. They are excluded from the Lausanne protections framework but actively contest the expansive reading, advocating instead for the restrictive reading that would subordinate minority institutions to general Turkish law. Their voice is loud in domestic politics but structurally outside the international treaty regime. Nationalist constituencies have successfully pressured the government on specific restrictions (Halki closure) but have not yet achieved a decisive shift to the restrictive reading.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_nationalist_constituencies, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of institutional continuity across a sovereignty discontinuity: the 1923 transition from Ottoman to Turkish statehood created a rupture in which minority institutions risked dissolution or forced assimilation. The expansive reading coordinates a settlement that allows minorities to retain pre-treaty governance structures, the Turkish state to preserve sovereignty over most domains, and the international treaty regime to provide a stable reference frame. This allows minority institutions to plan and organize over generations, knowing their fundamental structure is protected across political transitions and protected across different Turkish governments with different attitudes toward minorities.
% TRANSFER_FUNCTION: The treaty transfers institutional authority rather than material goods directly. The right to self-administer internal affairs, to control property accumulated before 1923, and to form clergy through theological schools flows from the international treaty regime to the named minorities, bypassing Turkish domestic law where it would otherwise apply. Turkish state authority is constrained in these specific domains; minority institutional autonomy is guaranteed by international obligation rather than Turkish law.
% ABSENT_VOICES: Turkish nationalist constituencies, which would contest the very idea of minority institutional autonomy as a violation of Turkish sovereignty and national integration, are structurally excluded from the protections framework. Anti-Western constituencies that view Lausanne as an imperialist imposition are also outside—they see the treaty not as protective but as a remnant of colonialism. Dissident voices within the minority communities themselves (those advocating complete assimilation, religious modernization, or state integration) are marginalized by the institutional leadership that holds the treaty interpretation monopoly and uses the treaty reading to enforce orthodoxy within their own communities.
% DISAPPEARANCE_RATIONALE: If the expansive reading and its protections disappeared overnight, the Patriarchate would lose its internationally guaranteed self-governance and property rights; Turkish law would apply directly to ecclesiastical administration, leadership selection, and clergy formation. Theological schools like Halki would face state control or closure (as happened in 1971 under Turkish pressure). The Armenian Church and Jewish communities would face similar reconfigurations. The institutional landscape of Turkey's religious minorities would reorganize around state-defined categories and the restrictive reading rather than pre-1923 continuity. This is not a minor reallocation—it is fundamental institutional restructuring that would eliminate centuries-old ecclesiastical governance models.
% FOUNDING_PROBLEM: The founding problem is the institutional discontinuity created by the 1923 Treaty of Lausanne itself. The transition from Ottoman multinational governance to a Turkish nation-state threatened the survival of non-Muslim institutional structures that had existed for centuries under Ottoman millet protections. Minorities feared dissolution of their churches, confiscation of properties, and prohibition of theological education. The Lausanne Treaty was designed to guarantee that this institutional inheritance would survive the state transition intact, providing international-law protection where domestic Turkish law might not.
% FOUNDING_PROBLEM_CORROBORATION: The Ecumenical Patriarchate and Armenian Church attest the founding problem remains live: ongoing restrictions on property rights, limitations on theological education (Halki seminary closure in 1971), and state pressure on institutional autonomy demonstrate that the problem has not gone away. European human rights bodies and academic scholarship on religious freedom in Turkey corroborate that minorities continue to face legal constraints on the exact functions the treaty was meant to protect. Reports from the ECHR, UN special rapporteurs, and international NGOs consistently document that the founding problem persists. Turkish government sources and restrictive-reading advocates counter that the founding problem was solved in 1923 and the institutional guarantees are now obsolete or unnecessarily constrain modern state governance and integration. No neutral external source can adjudicate this dispute; it is the site of the interpretive contest.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The expansive reading is modeled as a rope (genuine coordination with real beneficiaries, no pure extraction) because it solves a genuine coordination problem—institutional continuity across sovereignty transition—that would be costly to solve without a treaty reference frame. Extractiveness is LOW (0.28 at interval end) because the reading grants minority institutions genuine autonomy; it does not require them to transfer resources to a dominant party. Suppression is MODERATE (0.42) because the reading's persistence depends on Turkish state acceptance of constraints on its sovereignty—active enforcement is required, but suppression is directed at preventing Turkish reinterpretation rather than extracting from the minorities themselves. Theater is LOW (0.18): the treaty's institutional functions are real and continuous. The measurement series traces a slight rise in suppression through the mid-20th century (reflecting Turkish state pressure and the Halki closure in 1971) followed by stabilization, suggesting the constraint has found an equilibrium where the expansive reading persists despite periodic contestation. The modest rise in extractiveness reflects growing institutional costs of maintaining the reading against state pressure—not extraction FROM minorities but heightened institutional burden on them to defend their own guarantees. INDEPENDENCE OF CLAIM AND METRICS: The story claims rope (genuine coordination) and authors metrics consistent with that claim—low extraction, moderate enforcement cost, real function. The engine will compute whether this holds across all stakeholder seats; divergence would signal the constraint's structure differs from the claim.
 *
 * PERSPECTIVAL GAP:
 *   Different stakeholder seats should experience this constraint very differently. The Turkish government seat computes the constraint as a sovereignty constraint—it absorbs the cost of recognizing minority autonomy and faces diplomatic pressure if it deviates. The minority institutional seats compute coordination benefit with institutional vulnerability—they gain autonomy but are dependent on treaty reading persistence and could lose everything if the reading shifts to restrictive interpretation. The guarantor-state seats have weakened to observer status; they compute it as a monitoring and clarification role with minimal direct power. The excluded nationalist constituencies don't compute it as protective at all—they compute it as an impediment to Turkish integration and state sovereignty. The engine derives these per-seat computations from the structural data (power, exit, role); the seat divergence IS the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from the beneficiary/victim structure and exit options. The minority institutions (Patriarchate, Armenian Church, Jewish communities) are beneficiaries with trapped or constrained exit—they depend fundamentally on the treaty guarantee and cannot easily exit Turkish territory or institutional life. Their directionality is low (near 0.0, full beneficiary). The Turkish government is the agenda-setter, constrained by the reading but not victimized; it has mobile exit (could shift to restrictive interpretation, could invoke guarantor reading) and institutional power. Its directionality is moderate (near 0.5, symmetric cost-benefit). The guarantor states and human rights mechanisms are observers with analytical exit—they compute neutrally from the structure. The excluded nationalist constituencies are outside the constraint framework entirely. No agent here is a victim in the snare sense; the constraint is not built on coercive extraction of a target population. This structural composition supports the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the founding mandate outliving its function) is CONTESTED, not resolved. The expansive reading's mandate is institutional continuity across sovereignty transition—a problem that COULD be deemed solved (minorities have survived in Turkey) or COULD be deemed persistent (ongoing restrictions on the exact functions the treaty was meant to protect). Turkish government and restrictive-reading advocates argue mandatrophy: the 1923 threat is past, minorities have integrated or assimilated, the treaty protections are now obstacles to modern governance. The minority institutions and international human rights bodies argue the mandate is still live: the restrictions on property, theological education, and institutional autonomy demonstrate that the founding problem persists. This disagreement is structural, not a measurement error. If the constraint is classified as rope (coordination function still operating), the engine will flag the mandatrophy dispute as an omega question. If the contested status shifts to 'dead' with strong evidence that minorities no longer need institutional autonomy guarantees, the classification could shift toward piton (institutional inertia) rather than rope. The measurement trajectory shows suppression rising mid-century (Halki closure, property restrictions) and then stabilizing—suggesting the expansive reading has held its ground against Turkish pressure but at increasing institutional cost. This pattern is consistent with a rope under contestation rather than a straightforward mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_necessity_vs_cultural_survival,
    'Does the founding problem—ensuring institutional continuity of pre-1923 religious governance—remain a live necessity, or has institutional assimilation or cultural integration into Turkish society reduced its urgency?',
    'Ethnographic and institutional analysis of minority communities'' actual dependence on institutional autonomy for cultural reproduction and religious practice. Comparison with minority outcomes in jurisdictions with weaker institutional protections.',
    'If the founding problem is deemed dead (minorities survive and practice religion effectively without full institutional autonomy), the constraint''s classification could shift from rope (real coordination function) to piton (institutional inertia maintained by international and minority institutional leadership without genuine coordination necessity). If live, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_necessity_vs_cultural_survival, empirical, 'Whether the founding mandate of institutional continuity remains functionally necessary.').

omega_variable(
    reading_contestation_as_suppression_mechanism,
    'Is the measured suppression (0.42) primarily the cost of Turkish state pressure against the expansive reading, or does the suppression include internal minority institutional gatekeeping that enforces orthodox interpretation of the treaty and marginalizes dissident voices within the communities?',
    'Analysis of how minority institutional leadership mobilizes the treaty reading and whether the reading is deployed to suppress internal minority dissent or diversity (e.g., assimilationist voices, theological liberalization, gender equality in clergy selection).',
    'If suppression includes internal institutional gatekeeping, the constraint carries a hidden cost borne by minority populations themselves—those seeking different relationships to institutional authority. This would suggest the rope''s coordination function is partly purchased through suppression of internal alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_as_suppression_mechanism, empirical, 'Whether suppression is external (state pressure) or includes internal institutional gatekeeping.').

omega_variable(
    reading_shift_vulnerability,
    'How dependent is the constraint''s persistence on the current interpretive consensus? If Turkey shifted decisively to the restrictive reading (or if guarantor mechanisms became active on the guarantor reading), how quickly would the expansive reading''s protections collapse?',
    'Analysis of institutional precedent: the Halki seminary closure in 1971 demonstrates the Turkish government''s capacity to override expansive protections when it chooses. Hypothetical scenario analysis of what would happen if Turkish law explicitly codified the restrictive reading.',
    'The constraint''s vulnerability to interpretive shift is the defining feature of a treaty-dependent institutional guarantee. This is not the same as a snare''s vulnerability to exit—it is a vulnerability to reframing. If the engine computes a high reading-shift risk, the constraint classification remains rope (real coordination) but with elevated institutional fragility. This feeds downstream policy analysis about strengthening guarantor mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_shift_vulnerability, empirical, 'The institutional vulnerability of the expansive reading to interpretive displacement by the restrictive or guarantor readings.').

omega_variable(
    kernel_reading_decomposition,
    'Are the three readings of the Lausanne kernel genuinely DIFFERENT CONSTRAINTS instantiated by different interpretations of the same text, or are they better understood as three positions in a single debate about one constraint?',
    'Structural analysis: if the three readings instantiate different epsilon values, different beneficiary/victim structures, or different coordination functions, they are separate constraints. If they agree on ε and beneficiary structure but only disagree about narrative framing, they may be one constraint with perspective variance.',
    'This omega documents the decomposition principle itself: OQ-26 and ε-invariance require that genuinely different readings with genuinely different ε values be modeled as separate constraint stories. The three readings of Lausanne DO yield different ε values (expansive=0.28 rope, restrictive=higher extractiveness snare or unilateral subjugation, guarantor=supervised rope with different enforcement costs). Therefore they are three separate constraint stories, linked by network.affects_constraints. This omega confirms the decomposition was correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the three readings are separate constraints or one constraint with perspective variance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(laus_tr_t0, observed).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__expansive_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(laus_tr_t20, observed).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__expansive_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(laus_tr_t40, observed).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__expansive_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(laus_tr_t60, observed).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__expansive_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement_basis(laus_tr_t80, observed).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(laus_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(laus_be_t0, observed).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__expansive_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(laus_be_t20, observed).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__expansive_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(laus_be_t40, observed).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__expansive_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement_basis(laus_be_t60, observed).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__expansive_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement_basis(laus_be_t80, observed).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(laus_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__expansive_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(laus_su_t0, observed).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__expansive_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(laus_su_t20, observed).
narrative_ontology:measurement(laus_su_t40, lausanne_minority_protections__expansive_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement_basis(laus_su_t40, observed).
narrative_ontology:measurement(laus_su_t60, lausanne_minority_protections__expansive_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement_basis(laus_su_t60, observed).
narrative_ontology:measurement(laus_su_t80, lausanne_minority_protections__expansive_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(laus_su_t80, observed).
narrative_ontology:measurement(laus_su_t100, lausanne_minority_protections__expansive_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement_basis(laus_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel admits three structurally distinct readings, each with different ε values and different beneficiary/victim structures. This story models the expansive reading (institutional autonomy guarantee). The restrictive reading models the same treaty text as conferring only individual worship rights; it instantiates higher extractiveness and a snare-like structure where minority institutions are subordinated to Turkish law. The guarantor reading models international supervisory enforcement; it instantiates a supervised rope with different enforcement costs and international institutional beneficiaries. All three readings of the same text yield different constraint classifications because their ε values differ by structural design—what is protected, by whom, at what cost. Each is a separate constraint story, linked by network relationships documenting their kernel kinship and mutual contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
