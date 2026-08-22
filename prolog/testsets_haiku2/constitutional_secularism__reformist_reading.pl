% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Constitutional Duty to Eliminate Oppressive Religious Practices (Reformist Reading)
 *   domain: constitutional_law/religious_governance/social_reform
 *
 * SUMMARY:
 *   The reformist reading of constitutional secularism holds that the state
 *   has an affirmative duty to eliminate religious practices that oppress
 *   marginalized groups — women, lower castes, minorities within faith
 *   communities, LGBTQ+ individuals — even when those practices are defended
 *   as core to religious autonomy. This reading is most extractive toward
 *   religious conservatives and traditional leadership (who lose unilateral
 *   authority over practice) and most extractive from faith communities
 *   themselves (whose enforcement machinery is superseded). It benefits
 *   marginalized within-community members and secular reformers who view
 *   state intervention as legitimate social progress. The constraint is
 *   claimed as tangled_rope because it genuinely solves a coordination
 *   problem (protecting vulnerable persons trapped in oppressive hierarchies)
 *   while asymmetrically extracting from religious conservatives and
 *   institutional leadership. This is ONE reading of the contested
 *   constitutional secularism kernel; sibling readings (strict_neutrality and
 *   principled_intervention) instantiate different constraints with different
 *   ε values and beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Scheduled castes and religious minority women: primary beneficiaries, trapped exit, powerless
 *   - Religious conservatives and traditional leadership: primary payers, constrained exit, powerful-to-organized
 *   - Constitutional court and legislature: agenda setters enforcing the duty
 *   - Secular reformers and civil society: beneficiaries and observers with organized voice
 *   - Reform-minded religious communities: strategic beneficiaries leveraging state pressure
 *   - Excluded non-mainstream practitioners: absent but affected by state classification of oppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.78).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.71).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Constitutional Duty to Eliminate Oppressive Religious Practices (Reformist Reading)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/religious_governance/social_reform").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, 'd12c58b4-c288-464e-91cf-c874bd99d5b3').
narrative_ontology:cs_kernel_codification('d12c58b4-c288-464e-91cf-c874bd99d5b3', formalized).
narrative_ontology:cs_authority_grounding('d12c58b4-c288-464e-91cf-c874bd99d5b3', extraction).
narrative_ontology:cs_interpretation_layer_present('d12c58b4-c288-464e-91cf-c874bd99d5b3').
narrative_ontology:cs_reading_relation('d12c58b4-c288-464e-91cf-c874bd99d5b3', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('d12c58b4-c288-464e-91cf-c874bd99d5b3', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('d12c58b4-c288-464e-91cf-c874bd99d5b3', foundational, state_affirmative_duty_eliminate_oppression).
narrative_ontology:cs_axiom_status(state_affirmative_duty_eliminate_oppression, holdable).
narrative_ontology:cs_axiom_grounding('d12c58b4-c288-464e-91cf-c874bd99d5b3', state_affirmative_duty_eliminate_oppression, deontological).
narrative_ontology:cs_axiom('d12c58b4-c288-464e-91cf-c874bd99d5b3', foundational, religious_autonomy_secondary_to_marginalized_protection).
narrative_ontology:cs_axiom_status(religious_autonomy_secondary_to_marginalized_protection, holdable).
narrative_ontology:cs_axiom_grounding('d12c58b4-c288-464e-91cf-c874bd99d5b3', religious_autonomy_secondary_to_marginalized_protection, deontological).
narrative_ontology:cs_reference_frame('d12c58b4-c288-464e-91cf-c874bd99d5b3', religious_autonomy_as_constraint_on_state).
narrative_ontology:cs_drift_state('d12c58b4-c288-464e-91cf-c874bd99d5b3', contemporary_human_rights_advocacy, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d12c58b4-c288-464e-91cf-c874bd99d5b3', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, religious_minority_women).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, marginalized_within_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, reform_minded_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives_across_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, traditional_religious_leadership).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, faith_communities_resisting_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, secular_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to caste-based exclusion from temples, ritual participation, and leadership roles justified by religious hierarchy. State duty provides legal recourse for challenging discriminatory religious practices. Cannot exit caste identity or faith community without catastrophic social/economic costs. Legal protection is qualified: state recognition depends on demonstrating harm that religious conservatives dispute.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    powerless, generational, trapped, national).

% Subject to religiously-justified restrictions: forced/arranged marriage, custodial rights, inheritance rules, dress codes, temple access restrictions, ritual menstruation exclusions. State duty to eliminate oppressive practices offers legal pathway to challenge these rules. But accepting state protection threatens their religious identity and community belonging — they face a choice between safety-via-state and belonging-via-faith.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_minority_women, beneficiary,
    powerless, biographical, identity_locked, national).

% Include religious minorities within majority faiths, LGBTQ+ individuals subjected to hierarchy, dissidents, and people whose internal position makes them vulnerable to community punishment. State intervention offers legal standing but also visibility costs: seeking state protection can trigger retaliation by community authorities. Community exit is socially stigmatizing.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, marginalized_within_communities, beneficiary,
    powerless, biographical, constrained, national).

% Religious modernizers, reform theologians, progressive movements within traditions who agree oppressive practices should be eliminated. State enforcement strengthens their internal negotiating position by providing external legal pressure on conservatives. They benefit from allied state authority even as they maintain their religious identity. Can exit toward secularism if needed.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, reform_minded_religious_communities, beneficiary,
    moderate, generational, mobile, national).

% Hold that traditional religious practices are constitutive of faith and that state interference violates religious autonomy and self-determination. They experience the constraint as coercive delegitimization of their authority and interpretation. Compliance costs are substantial: organizational resource drain, loss of internal authority, erosion of congregational legitimacy. Constrained exit: cannot easily exit their religious tradition or their conservative convictions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives_across_communities, payer,
    moderate, generational, constrained, national).

% Institutional and interpretive authority (priests, imams, gurus, institutional heads) whose power to enforce tradition is superseded by state law. Faces legal liability, congregational fragmentation where enforcement fails, and institutional resource drain from litigation and compliance. Must justify practices to state judges, not just to community members.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, traditional_religious_leadership, payer,
    powerful, generational, constrained, national).

% Organized religious institutions (temples, mosques, churches, synagogues, gurdwaras) viewing state intrusion as delegitimizing their self-regulatory authority. They experience the constraint as forcing demonstrable state compliance under threat of legal consequence. Institutional solidarity costs are high: maintaining tradition while accommodating state rulings fragments community coherence.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, faith_communities_resisting_intervention, payer,
    organized, generational, constrained, national).

% Adjudicates what counts as oppressive religious practice; issues rulings that supersede religious autonomy claims. Exercises state coercive power to enforce the duty. Manages tension between legitimate religious autonomy and protection of marginalized groups. Holds power to define scope and reach of the duty through case-by-case reasoning.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Enacts laws embodying the reformist duty to eliminate oppressive practices; appropriates enforcement resources and mechanisms. Reflects majoritarian preferences and political coalitions. Holds power to expand or narrow scope of the duty through legislation, to fund or defund enforcement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, legislative_majority, agenda_setter,
    institutional, generational, analytical, national).

% Intellectuals, advocates, and political movements believing state-directed elimination of religiously-justified oppression is legitimate social reform. Benefit from expanded state authority and vindication of secular-modernist frameworks. Lobby for broad enforcement and view the constraint as advancing equality. Can exit toward different reform strategies if needed.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, secular_reformers, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, secular_reformers, agenda_setter).

% Indigenous practitioners, minority sect members, syncretic believers, and heterodox communities whose voices are rarely heard in constitutional proceedings. Absent from beneficiary/payer frames but vulnerable to state classification of their practices as oppressive or deviant. Risk experiencing state enforcement as cultural suppression.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, excluded_non_mainstream_practitioners, excluded,
    powerless, biographical, trapped, national).

% Document harms from religious oppression; provide legal aid to marginalized groups; monitor state enforcement. Independent analytical position: assess both whether oppressive practices are being eliminated and whether state enforcement itself is generating new harms, exceeding legitimate bounds, or misidentifying oppression.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, civil_society_human_rights_organizations, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, traditional_religious_leadership).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for protecting marginalized individuals and groups from religiously-justified oppression while recognizing that some religious autonomy must be respected. Solves the problem of how a secular state coordinates protection of vulnerable persons within religious communities without either abandoning them to internal domination or dissolving all religious self-governance.
% TRANSFER_FUNCTION: Transfers decision-making authority over religiously-justified practices from internal community religious leadership to constitutional courts and state legislatures. Transfers the capacity to challenge oppressive practices from marginalized individuals (who lack power within their faith communities) to the state legal system. Generates enforcement costs borne by religious institutions and conservatives.
% ABSENT_VOICES: Non-mainstream religious practitioners, indigenous practitioners, and minority sect members are typically absent from constitutional proceedings where the reformist duty is debated. They lack the organized voice and legal resources of majority religions or secular reformers. Religious women in communities where patriarchy is contested from within are often absent as separate speakers — they are spoken for by either conservative or reformer camps.
% DISAPPEARANCE_RATIONALE: If the state duty to eliminate oppressive practices disappeared overnight, marginalized groups within religious communities would lose the primary legal recourse they possess (state enforcement). Communities would revert to internal dispute resolution, which typically favors traditional power structures. Oppressive practices would re-normalize absent external pressure. Conversely, religious institutions would regain unilateral authority over internal discipline and practice without fear of state intervention.
% FOUNDING_PROBLEM: Religious communities practice exclusion, hierarchy, and bodily restriction justified by sacred texts and traditions — practices that oppress women, lower castes, minorities within faiths, and LGBTQ+ members. Marginalized individuals trapped within these communities lack the power to reform from inside and face legal barriers if they seek state redress (religious autonomy doctrine insulating practices from scrutiny).
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations document ongoing oppressive practices (forced marriage, caste discrimination in temples, menstruation restrictions, female genital mutilation justified religiously) across multiple faith traditions. Marginalized group members and reform-minded theologians attest the practices persist and cause identifiable harms. Religious conservatives dispute the characterization of these as oppressive rather than constitutive of faith identity.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58→0.78 over the interval) because the constraint's enforcement increasingly reduces religious institutional autonomy and transfers decision-making authority to the state. The rise reflects accumulated case law and legislative expansion of what counts as oppressive and subject to state remedy. Suppression is substantial (0.71 at interval end) because religious institutions actively resist state intrusion and many conservatives view enforcement as delegitimizing their religious authority. The constraint persists despite high resistance because the beneficiary set includes both powerless marginalized groups (who provide moral authority) and powerful secular reformers (who provide political resources). Theater is moderate (0.41): genuine protection of marginalized groups occurs, but state enforcement is increasingly performative in some domains — courts issue rulings that reshape practice symbolically without fully eliminating oppressive mechanisms. The measurement grid uses one shared time axis so each metric is authored at every point. Suppression_requirement rises as the constraint matures because enforcement machinery must harden to overcome growing institutional resistance from organized religious bodies.
 *
 * PERSPECTIVAL GAP:
 *   Conservative religious leaders on one hand and marginalized within-community members on the other occupy radically different structural positions within the same constraint. The constraint is presented by reformers and courts as protection (marginalized frame) but experienced by conservatives as coercion (conservative frame). Neither frame is false; both are structural facts about the constraint's asymmetry. Marginalized beneficiaries face a specific perceptual gap: state law offers protection from oppressive practices but threatens their religious identity simultaneously — they must choose between safety-via-state and belonging-via-faith. This gap is not a disagreement about facts; it is a structural bind the constraint creates.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled castes and religious minority women are beneficiaries (d near 0.0: they gain protection without running the system) but identity_locked (they cannot easily exit their religious identity even as state law offers protection against oppressive practices). Directionality derivation: beneficiary role, powerless power, identity_locked exit → d in range [0.05, 0.25], depending on whether identity lock is treated as exit-reducing (moving d toward 0.0) or as extracting a cost (identity threat pushing d toward 0.5). This story authors identity-lock suppression omegas to document the ambiguity. Religious conservatives are payers (d near 1.0: they bear enforcement costs and loss of authority) with constrained exit and moderate power. Directionality: payer role, moderate power, constrained exit → d in range [0.75, 0.90]. No override needed; structural derivation is sound. Constitutional court and legislature are agenda-setters with analytical exit options and institutional power — they operate largely outside the constraint's directionality computation (observer/analytical seats). Secular reformers are beneficiaries with powerful institutions and mobile exit — d in range [0.15, 0.35]: they benefit from expanded state authority but are not trapped by it. The asymmetry is structural: conservative payers face high extraction, marginalized beneficiaries face protection paired with identity threat, reformers face amplified institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading avoids the false-summits mislabeling trap by naming what actually gets coordinated (protecting marginalized persons from oppression within communities) versus what actually gets extracted (authority and autonomy from religious institutions and conservatives). The coordination function is real: in the absence of state duty to intervene, marginalized individuals within oppressive hierarchies have no recourse except community-internal reform (slow or blocked) or exit (socially/economically catastrophic). The state duty solves that problem. But the solution is paid for primarily by religious institutions, not by marginal cost of enforcement. The payers did not consent to this arrangement; they experience it as imposed. This is the tangled_rope structure: genuine coordination payoff for one set (marginalized protection) married to asymmetric extraction from another set (conservative authority loss) through the same mechanism (state enforcement against practices the payers view as legitimate religious self-governance). The constraint avoids pure-extraction status (snare) because the coordination function is substantial and beneficial; it avoids pure-coordination status (rope) because extraction is high and asymmetric. It earns tangled_rope precisely by possessing both genuine coordination (marginal value) and genuine extraction (borne asymmetrically by conservatives) through the same enforced structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_suppression_internalization,
    'For marginalized beneficiaries (especially religious minority women and lower castes), is suppression of their autonomy within oppressive religious practices structural (imposed by law and hierarchy) or internalized (they have adopted the belief that oppressive practices are legitimate)?',
    'Post-intervention autonomy studies: track how quickly individuals exercise new legal protections after court rulings; measure whether removal of legal barriers to exit correlates with actual exit or with continued participation. Compare regions with aggressive vs. minimal state enforcement to see if internalized suppression persists after structural barriers are removed.',
    'If suppression is mostly structural, the state duty is high-leverage: removing legal barriers enables rapid behavioral change. If suppression is mostly internalized, the constraint''s effectiveness is limited by psychological and cultural factors that law cannot quickly reach — the measured suppression (0.71) may overstate actual capacity for change. This affects whether the constraint is genuinely liberatory or performatively so.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_internalization, empirical, 'Whether beneficiary-side suppression is structural or internalized, affecting constraint effectiveness.').

omega_variable(
    oppression_definition_epistemology,
    'By what epistemic standard does the state determine whether a religious practice is oppressive? Whose harm counts as oppression? Is harm defined by objective condition, by the practitioner''s perception, or by a secular/reformist standard that may not be shared by the religious tradition?',
    'Examine court decisions in contested cases (menstruation restrictions, arranged marriage, ritual practices disputed within the tradition itself) to see whether harm is defined by external secular standard or by reference to how marginalized members experience the practice within their own tradition''s framework.',
    'If the state applies an external secular standard of harm, the constraint becomes paternalistic: state judges what is oppressive for religious communities, not marginalized group members themselves. If harm is defined by marginalized members'' own testimony, the constraint is legitimately protective. This distinction affects whether the constraint is truly aligned with beneficiary interests or whether it imposes a particular vision of liberation. High state paternalism would increase effective extraction from payers while reducing actual liberation for beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oppression_definition_epistemology, conceptual, 'Whether oppression is defined by state secular standard or by marginalized members'' testimony, affecting legitimacy.').

omega_variable(
    state_enforcement_overreach_risk,
    'Does enforcement of the duty to eliminate oppressive practices create a new form of oppression: state classification and coercion of minority religious practices as oppressive when practitioners do not experience them as such?',
    'Documentation of false-positive enforcement: practices ruled oppressive by courts but defended by practitioners within the tradition; tracking of instances where marginalized members reject state intervention as paternalistic or culturally destructive. Compare enforcement patterns across majority vs. minority religions to measure whether state power asymmetrically targets minority practices.',
    'If state enforcement frequently misidentifies or overgeneralizes oppression, the constraint generates new harms (state coercion of religious minorities) while addressing old ones. The effective extraction on payers rises and may extend to marginalized groups themselves if they experience state intervention as cultural assault. This would shift classification toward snare (pure extraction covering paternalistic harm) from tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_enforcement_overreach_risk, empirical, 'Risk of state enforcement overreach creating new oppression of minority religious practices.').

omega_variable(
    religious_autonomy_kernel_reading_contest,
    'This is ONE reading of the constitutional_secularism kernel — the reformist reading, which prioritizes elimination of oppression over religious autonomy. Are the sibling readings (strict_neutrality, principled_intervention) genuinely coexisting live positions, or does the reformist reading structurally foreclose them within this state''s constitutional framework?',
    'Historical/doctrinal analysis: can strict_neutrality judges and principled_intervention judges coexist on the same bench interpreting the same constitutional text? Or does adoption of reformism as binding doctrine logically require rejection of the other readings? Check whether constitutional amendment would be required to switch readings or whether competing readings can be held simultaneously by different institutional actors.',
    'If readings coexist (different judges, different eras, different factions), they remain live constraint siblings. If reformism forecloses others, the kernel codification is effectively monolith-convergent: the reformist duty becomes the only coherent reading, and alternative positions are untenable within the framework. This affects how the constraint family is modeled and whether judicial reversals are reversions (coexistence) or defeats (foreclosure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_autonomy_kernel_reading_contest, conceptual, 'Whether sibling readings genuinely coexist or whether reformism forecloses them within the constitutional framework.').

omega_variable(
    marginalized_internal_agency_vs_state_substitution,
    'To what degree does state enforcement of the duty to eliminate oppressive practices empower marginalized individuals to reform their own communities (internal agency) versus substituting state authority for community internal decision-making (state substitution)?',
    'Track whether enforcement enables marginalized members to lead internal reform movements or whether it centralizes authority in state courts and legislatures. Measure outcomes: practices eliminated via community negotiation and internal change vs. practices eliminated purely by external legal mandate with community resistance.',
    'High internal agency: marginalized groups gain capacity to reshape their own traditions, and the constraint is genuinely emancipatory. High state substitution: marginalized groups become passive beneficiaries of state paternalism, and the constraint reduces their autonomy even as it protects them from oppressive practices. This affects whether the constraint benefits are sustainable (internally owned) or fragile (dependent on state will).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_internal_agency_vs_state_substitution, empirical, 'Balance between empowering marginalized internal reform versus substituting state authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_secularism__reformist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__reformist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__reformist_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__reformist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__reformist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t35, constitutional_secularism__reformist_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(cons_tr_t35, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_secularism__reformist_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__reformist_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__reformist_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__reformist_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__reformist_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t35, constitutional_secularism__reformist_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(cons_be_t35, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_secularism__reformist_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__reformist_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__reformist_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__reformist_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__reformist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t35, constitutional_secularism__reformist_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(cons_su_t35, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__reformist_reading, 0.14).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

% DUAL FORMULATION NOTE:
% The constitutional_secularism kernel decomposes into three structurally distinct constraints, each a different reading of how the state should relate to religious communities' internal practices. The reformist_reading (this story) is most extractive on religious autonomy and most protective of marginalized groups. The strict_neutrality_reading treats state equidistance from all religions as the duty; eliminates oppressive practices only when they breach generally applicable law. The principled_intervention_reading permits state intervention for social reform under narrower conditions than reformism requires. All three share the same kernel (state-religion relationship) but author different ε values, different beneficiary/victim structures, and different classifications. Each story stands alone as a complete constraint; the network links them as sibling readings of the same constitutional contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, powerless, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
