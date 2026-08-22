% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Qur'anic Gender Verses: Contextual-Egalitarian Reading
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   The contextual-egalitarian reading interprets Qur'anic gender-related
 *   verses (4:11 on inheritance, 2:282 on testimony, 4:34 on guardianship) as
 *   historically situated responses to 7th-century Arabian social conditions,
 *   not as timeless divine ordinances. Under this reading, the overarching
 *   principle of Qur'anic equity (maqasid al-shariah) permits and requires
 *   reinterpretation when foundational circumstances change. This reading has
 *   gained substantial institutional support among reformist scholars, NGOs,
 *   and progressive religious communities since the mid-20th century. It
 *   directly contests traditionalist and literalist readings held by
 *   conservative religious authorities and identity-locked believer
 *   communities. The extractiveness measure (0.38) reflects moderate but
 *   asymmetric transfer: reformist scholars and rights advocates gain
 *   interpretive authority and policy influence; patriarchal authorities lose
 *   discretionary power; women gain structural legal claims. Suppression is
 *   moderate (0.42) because the reading's persistence requires active
 *   contestation against powerful traditionalist opposition and some social
 *   resistance in conservative communities. Theater ratio is low (0.28)
 *   because the interpretive work is substantive, though defensive rhetoric
 *   and institutional performance do feature.
 *
 * KEY AGENTS:
 *   - Reformist scholars: institutional power, mobile, author and defend the contextual reading through academic and theological work
 *   - Rights-based NGOs: organized, mobile, mobilize the contextual reading for policy advocacy and institutional reform
 *   - Women seeking equal rights: moderate power, constrained, benefit from structural legal claims the reading enables
 *   - Patriarchal traditional authorities: powerful, constrained by institutional identity, lose interpretive monopoly and discretionary power
 *   - Literal interpreter communities: moderate power, identity-locked, experience theological and social destabilization
 *   - Secular legal systems: institutional power, analytical, implement contextual readings in pluralistic legal frameworks
 *   - Conservative religious authorities: powerful, trapped, forced to defend literalist premises on terms they did not author
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.38).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.42).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.38).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Qur'anic Gender Verses: Contextual-Egalitarian Reading").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, 'cf277498-1d68-430e-afc6-d75122febca9').
narrative_ontology:cs_kernel_codification('cf277498-1d68-430e-afc6-d75122febca9', fixed_text).
narrative_ontology:cs_authority_grounding('cf277498-1d68-430e-afc6-d75122febca9', lineage).
narrative_ontology:cs_interpretation_layer_present('cf277498-1d68-430e-afc6-d75122febca9').
narrative_ontology:cs_reading_relation('cf277498-1d68-430e-afc6-d75122febca9', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('cf277498-1d68-430e-afc6-d75122febca9', quranic_gender_verses__progressive_abrogation, influences).
narrative_ontology:cs_axiom('cf277498-1d68-430e-afc6-d75122febca9', foundational, historical_contingency_of_gender_rules).
narrative_ontology:cs_axiom_status(historical_contingency_of_gender_rules, holdable).
narrative_ontology:cs_axiom_grounding('cf277498-1d68-430e-afc6-d75122febca9', historical_contingency_of_gender_rules, empirically_contingent).
narrative_ontology:cs_axiom('cf277498-1d68-430e-afc6-d75122febca9', foundational, maqasid_permit_contextual_reinterpretation).
narrative_ontology:cs_axiom_status(maqasid_permit_contextual_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('cf277498-1d68-430e-afc6-d75122febca9', maqasid_permit_contextual_reinterpretation, deontological).
narrative_ontology:cs_reference_frame('cf277498-1d68-430e-afc6-d75122febca9', quranic_equity_principle_as_hermeneutical_master_key).
narrative_ontology:cs_drift_state('cf277498-1d68-430e-afc6-d75122febca9', contemporary_rights_advocacy_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('cf277498-1d68-430e-afc6-d75122febca9', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_seeking_equal_rights).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_traditional_authorities).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, literal_interpreter_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, secular_legal_systems).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, progressive_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and theological scholars who author and defend the contextual-egalitarian reading. They conduct historical-critical exegesis, publish interpretive works, train students, and command authority in educational and progressive religious institutions. Their interpretive framework gains legitimacy from maqasid jurisprudence and comparative hermeneutics. They face social and institutional pushback from traditionalist bodies but retain intellectual mobility and academic freedom in secular and progressive educational settings.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    institutional, generational, mobile, global).

% International and local human rights organizations, women's rights groups, and interfaith advocacy bodies that use the contextual-egalitarian reading to press for legal reforms (inheritance equity, testimony equality, marriage contract autonomy). They mobilize this reading as a legitimacy anchor for advocacy, gain standing in policy debates, and shape national laws and institutional policies. They operate at high visibility and face legal and social resistance in conservative jurisdictions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, agenda_setter).

% Women whose legal and social standing depends on which scriptural interpretation prevails. Under the contextual-egalitarian reading, they gain structural claims to equal inheritance shares, valid testimony in court without corroboration, independent marriage contract negotiation, and equal guardianship status. Under literal-hierarchical or traditional readings, these claims are foreclosed. Their exit options are constrained by family law, property law, and social structures that bind them to interpretive communities.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, women_seeking_equal_rights, beneficiary,
    moderate, biographical, constrained, global).

% Established religious authorities (imams, muftis, traditionalist scholars), patriarchal family structures, conservative courts, and institutions whose power and legitimacy derive from literal or traditional gender-hierarchical readings. The contextual-egalitarian reading undermines their interpretive monopoly, forces them to justify male-privileged rules against maqasid-based counter-arguments, and shifts policy outcomes away from inherited discretionary power. Their exit is constrained by institutional identity and doctrinal commitment.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_traditional_authorities, payer,
    powerful, generational, constrained, global).

% Communities of believers and scholars whose theological identity and social practice rest on literal or traditionalist readings of gender verses. The contextual-egalitarian reading treats their foundational premises as historically contingent rather than timeless ordinance, creates internal doctrinal pressure (reformist voices within their own communities adopt maqasid framing), and erodes the inherited interpretive authority they rely on to maintain social cohesion. Exit is identity-locked: rejecting the literal reading would destabilize their entire theological and social framework.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, literal_interpreter_communities, payer,
    moderate, generational, identity_locked, global).

% State legal systems and constitutional courts that apply contextual-egalitarian readings to interpret religious law within secular jurisprudence or pluralistic frameworks. These institutions gain legitimacy claims for gender-equal law codes by citing reformed Islamic interpretation, avoid the diplomatic cost of dismissing Islamic law entirely, and sidestep pressure from conservative religious authorities by channeling them through interpretive reform. They remain analytical observers in theological debates but occupy the position of ultimate arbiter in legal implementation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, secular_legal_systems, beneficiary,
    institutional, generational, analytical, national).

% Faith communities, congregations, and religious organizations that embrace contextual-egalitarian readings and restructure their internal governance, marriage practices, and inheritance protocols accordingly. They experience increased internal legitimacy (alignment with members' lived values), attraction of progressive believers, and reduced tension with secular equality norms. They face social ostracism from traditionalist communities and legal complexity navigating plural legal systems.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, progressive_religious_communities, beneficiary,
    moderate, biographical, mobile, global).

% Major theological schools, hierarchical religious institutions, and powerful conservative movements whose entire legitimacy framework depends on literal or traditionalist readings remaining unchallengeable. They are structurally excluded from the interpretive authority the contextual-egalitarian reading distributes: they are forced to engage in debate they did not author, on terms (historical contingency, maqasid reinterpretation) that destabilize their own foundations. Their exclusion is not from the conversation but from the privilege of unmediated authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, conservative_religious_authorities, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared hermeneutical framework for understanding Qur'anic gender-related verses as products of their historical moment, unified by overarching principles of equity (maqasid) rather than as timeless, immutable laws. Solves the coordination problem of reconciling scriptural authority with historical change and contemporary gender equality norms: both can be honored if the verses are read as progressive *for their time* rather than as fixed ordinances.
% TRANSFER_FUNCTION: Transfers interpretive authority from traditionalist and literalist scholars (who control the reading) to reformist scholars and rights-based organizations (who author and defend the contextual reading). Simultaneously transfers structural legal rights from males (who hold privileged testimony, inheritance, guardianship under literalist readings) to women (who gain equal claims under contextual readings). The arrangement also transfers institutional power from conservative religious courts to secular and progressive legal systems that adopt contextual interpretations.
% ABSENT_VOICES: Conservative religious scholars and patriarchal-traditionalist leaders are not excluded from the conversation but are forced to defend literalist premises against hermeneutical counter-arguments they did not initiate. Their 'absence' is from the privileged position of unquestioned authority they previously held. Literalist communities whose religious identity fuses with traditional gender hierarchy are structurally positioned as opponents rather than partners—they would argue for timeless divine ordinance and against historical contingency, but that argument is no longer treated as self-evident.
% DISAPPEARANCE_RATIONALE: If the contextual-egalitarian reading disappeared and literal or traditionalist readings achieved global hegemony, inheritance law, testimony rules, guardianship structures, and family law in Muslim-majority jurisdictions would shift back toward male-privileged asymmetries. If it succeeded and became dominant, women's legal status across Islamic jurisprudence would shift toward formal equality. The constraint's persistence directly determines legal and social arrangements affecting billions of people.
% FOUNDING_PROBLEM: 7th-century Arabia presented women with severe social vulnerabilities: limited property rights, economic dependence on male kinship, and vulnerability in conflict. The gender-related verses (e.g., inheritance differentiation, testimony rules, guardianship provisions) were contextual responses to those specific problems, improving women's position relative to pre-Islamic Arab customary law. Later centuries inherited these verses as fixed law even after the specific problems they addressed were partially or wholly solved by social change.
% FOUNDING_PROBLEM_CORROBORATION: Contextual-egalitarian scholars (reformist theologians, historical-critical exegetes outside traditionalist establishments) attest the founding problem was specific to 7th-century Arabian socioeconomic conditions. Rights-based NGOs and women's rights advocates attest that the problem is substantially solved in contemporary contexts through education, property law, and legal equality provisions. Conservative traditionalist authorities attest the problem remains live—that women require male guardianship and differential rules for their own protection. Secular scholars of Islamic history and anthropologists outside religious authority structures affirm the contextual-historical reading; no corroboration comes from traditionalist authorities themselves.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).
:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the reading redistributes authority and legal rights rather than merely suppressing alternatives. The gain is concentrated (reformist scholars, rights NGOs) but not unlimited—they must continuously defend the reading against traditionalist counter-argument and cannot simply impose it. Suppression rises from 0.25 to 0.42 over the interval as traditionalist resistance hardens and the reading gains ground in law and policy, requiring reformists to actively maintain interpretive space. Theater ratio remains low (0.28) because the hermeneutical work is substantive exegesis and legal argument, though some institutional performance does occur (conferences, scholarly publications, policy briefs). Accessibility collapse (0.45) is moderate: the contextual reading is intellectually accessible (historical-critical method is teachable) but requires abandoning literal-text authority, which is high-cost for identity-locked communities. Resistance is high (0.72) because the reading directly challenges foundational premises of literalist and traditionalist communities and faces institutional opposition from powerful religious authorities. The measurement series show extractiveness rising steeply early (0.22 → 0.37 over first 25 time points) as the reading gains institutional footing, then plateauing (0.37 → 0.38 in final 25 points) as it reaches equilibrium against persistent traditionalist opposition.
 *
 * PERSPECTIVAL GAP:
 *   From reformist scholars' position, the arrangement is genuine hermeneutical coordination solving a real problem: reconciling scriptural authority with historical change and gender equality norms. From patriarchal traditionalist positions, the same reading operates as interpretive domination undermining literal divine ordinance. From women's positions, it is unambiguously beneficial (structural rights redistribution). From conservative authorities' positions, it is a threat to theological and institutional coherence. The engine should compute per-seat classifications reflecting these divergent structural relationships: reformist scholars sit near beneficiary/agenda-setter (d low-moderate), women near beneficiary (d low), traditionalist authorities near target (d high), literal communities near target with identity-lock dynamics (d high, exit constrained). The claim is tangled-rope (genuine coordination + asymmetric extraction of authority); metrics show moderate extractiveness and active enforcement, consistent with that claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars benefit from the reading (gain interpretive authority, institutional position, influence in policy); they are beneficiaries but also agenda-setters—they author and defend the frame. Women benefit structurally (equal inheritance, testimony, guardianship claims) but remain constrained in exit (bound to family law, property systems); they are beneficiaries but not decision-makers. Patriarchal authorities lose discretionary power and interpretive monopoly; they are victims in the structural sense (extraction of authority) but retain institutional power. Literal interpreter communities are victims in a different sense: their entire theological and social identity is destabilized; their exit is identity-locked—they cannot simply adopt a different reading without cognitive and relational collapse. The reading requires active enforcement by reformist scholars (continuous hermeneutical argument), rights organizations (policy advocacy), and secular legal systems (implementation); without these efforts, traditionalist readings would revert to dominance. This is the tangled-rope structure: genuine coordination (historical hermeneutics + equity principles unified) riding on asymmetric extraction (authority transferred from traditionalists to reformists, legal status transferred from males to females).
 *
 * MANDATROPHY ANALYSIS:
 *   The contextual-egalitarian reading does NOT exhibit mandatrophy. The founding problem—7th-century women's vulnerability and the verses as contextual responses—is clearly articulated and historically defensible. The problem status is contested (traditionalists argue women still require male guardianship; reformists argue the problem is solved through education and law), which is appropriate: the disagreement is over whether the problem remains live, not over whether the reading forecloses the original function. The reading's persistence depends on active reformist and advocacy work, not theatrical maintenance—hermeneutical exegesis is substantive, policy advocacy is concrete. Theater ratio is low (0.28) and does not rise significantly over the interval, indicating functional work rather than performative decay. The reading does not suffer from the piton pattern (function atrophied but constraint persists through inertia). Mandatrophy would apply if literalist readings persisted while the coordination problem they addressed was demonstrably solved and no party actively maintained them—that is not the case here; traditionalist authorities remain actively engaged in defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem that prompted the differentiated rules—women''s severe economic vulnerability in 7th-century Arabia—genuinely solved in contemporary contexts where the contextual reading applies, or do women still require male-privileged rules for substantive protection?',
    'Empirical assessment of women''s property rights, economic autonomy, legal protection, and lived vulnerability in jurisdictions with equal-rights legal codes versus those with male-guardianship-privileged codes, controlling for development level and enforcement quality.',
    'If solved: the contextual reading''s mandate is complete and further male-privilege rules are indefensible as responsive to the original problem. If unsolved: traditionalist claims that women require protective differentiation gain empirical ground. If partially solved (equality on paper, enforcement gaps): both readings retain defensibility and contest shifts to institutional implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the original 7th-century problem prompting gender differentiation persists in contemporary contexts.').

omega_variable(
    maqasid_principle_authority,
    'Does the overarching principle of Qur''anic equity (maqasid al-shariah) genuinely authorize reinterpretation of specific verses when circumstances change, or is maqasid itself contested and does not settle hermeneutical disputes?',
    'Scholarly consensus analysis: what do traditionalist, literalist, contextual, and progressive scholars actually assert maqasid permits? Is there genuine disagreement about maqasid''s scope and authority, or is maqasid itself a site of dispute?',
    'If maqasid unambiguously authorizes reinterpretation, the contextual reading gains hermeneutical certainty. If maqasid is itself contested, the reading''s authority depends on which interpretation of maqasid prevails—the hermeneutical ground shifts one level deeper.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maqasid_principle_authority, conceptual, 'Whether maqasid principles provide genuine hermeneutical authority for contextual reinterpretation or remain themselves contested.').

omega_variable(
    identity_lock_escape_pathway,
    'For literal believer communities whose religious identity is fused with literalist readings, what would enable exit from identity-locked status? Is it cognitive reframing, institutional permission to reinterpret, lived experience of equal-rights outcomes, or something else?',
    'Longitudinal study of believer communities that have shifted from literalist to contextual readings (or vice versa): what triggered the shift? Were exits gradual or sudden? Did exit require institutional authorization from trusted authorities?',
    'If exits are possible through cognitive reframing, the constraint''s persistence depends on continuous cognitive reinforcement (theater ratio rises). If exits require institutional permission, the constraint''s persistence depends on reformist scholars'' willingness to grant permission (authority remains key). If exits are nearly impossible, identity-locked believers represent a permanent victim class for this constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_escape_pathway, empirical, 'What pathways enable exit from identity-locked literalist reading status.').

omega_variable(
    reading_kernel_relation_clarification,
    'Is the kernel the TEXT (4:11, 2:282, 4:34 as written) or the DOCTRINE (male-privileged gender law as historically interpreted)? Different answers change which readings truly engage the same kernel versus merely use the same language for different claims.',
    'Semantic analysis of how each reading (contextual-egalitarian, literal-hierarchical, progressive-abrogation) treats the referent: do they all claim to interpret the same Qur''anic passage, or does the literal reading claim the verses ARE law while the contextual reading claims they are historical documents requiring interpretation?',
    'If kernel is text, all three readings engage the same object and true hermeneutical contest occurs. If kernel is doctrine, the literal reading may not be engaged in the same kernel at all—it is asserting rather than interpreting. This shifts the reading_relations from coexists_with to something more asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_relation_clarification, conceptual, 'What serves as the kernel that all readings claim to interpret: the scriptural text or the doctrine it grounds.').

omega_variable(
    authority_structure_reform_stability,
    'When reformist scholars gain interpretive authority under the contextual-egalitarian reading, do they remain open to further reinterpretation and challenge, or do they crystallize into a new interpretive orthodoxy that suppresses alternative readings (including progressive-abrogation)?',
    'Historical analysis of reformist-led jurisprudential schools: do they permit internal dissent and continued hermeneutical evolution, or do they entrench new orthodoxies? Is the contextual reading fundamentally pluralistic or does it contain seeds of its own ossification?',
    'If reformists remain genuinely open to challenge, the constraint''s character changes over time (tangled-rope may devolve to rope or evolve to piton). If reformists entrench new orthodoxy, the structural asymmetry of authority persists—a new beneficiary class simply replaces the old, and the constraint remains tangled-rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_structure_reform_stability, empirical, 'Whether contextual-egalitarian interpretive authority, once gained, remains genuinely open to further challenge or crystallizes into new orthodoxy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__contextual_egalitarian, theater_ratio, 8, 0.16).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__contextual_egalitarian, theater_ratio, 16, 0.21).
narrative_ontology:measurement(qura_tr_t25, quranic_gender_verses__contextual_egalitarian, theater_ratio, 25, 0.26).
narrative_ontology:measurement(qura_tr_t37, quranic_gender_verses__contextual_egalitarian, theater_ratio, 37, 0.28).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__contextual_egalitarian, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 16, 0.32).
narrative_ontology:measurement(qura_be_t25, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 25, 0.37).
narrative_ontology:measurement(qura_be_t37, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 37, 0.38).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(qura_su_t25, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(qura_su_t37, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 37, 0.42).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__contextual_egalitarian, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, islamic_family_law__inheritance_rights).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, islamic_family_law__women_testimony).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, islamic_family_law__guardianship_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel quranic_gender_verses. The contextual-egalitarian reading decomposes the gender verses into historically situated responses to 7th-century Arabian vulnerabilities, unified by overarching equity principles. The literal-hierarchical reading treats the same verses as timeless divine ordinances establishing male-privileged status. The progressive-abrogation reading treats them as incomplete trajectory superseded by later egalitarian principles. Each reading produces a different ε value and beneficiary/victim structure. They are linked via network.affects_constraints because they compete over interpretive authority and influence the same downstream legal constraints (inheritance, testimony, guardianship rules in Islamic family law systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, organized, 0.15).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
