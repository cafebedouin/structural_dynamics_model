% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract under State Law (Secular Contractual Reading)
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the secular contractual reading of family
 *   law authority: marriage is a civil contract between autonomous
 *   individuals, validated by state registration, with gender-symmetric
 *   property and custody rights, and divorce available by civil process
 *   without religious authority consent. This reading coexists with four
 *   sibling readings grounded in Christian (sacramental), Islamic (nikah),
 *   Hindu (samskara), and Zoroastrian traditions. Each reading instantiates a
 *   different constraint because they differ in who adjudicates marriage
 *   validity, what constitutes a binding marriage, who can marry whom, and
 *   what dissolution requires. The secular contractual reading treats
 *   marriage as a voluntary association of autonomous individuals; the
 *   religious readings ground marriage in cosmic or communal commitments
 *   beyond individual choice. The constraint is CLAIMED as rope (genuine
 *   coordination function solving a real pluralism problem); the authored
 *   metrics reflect modest but real extraction as the state gates legitimate
 *   marriage recognition and subordinates religious authorities. This
 *   reading's chief extractive cost: religious traditionalists lose authority
 *   over marriage law and face the subordination of their frameworks to state
 *   processes.
 *
 * KEY AGENTS:
 *   - State civil authority: agenda-setter, defines valid marriage, enforces secular rights regime
 *   - Secular married individuals: beneficiaries, gain rights independent of religious approval
 *   - Religious traditionalist communities: payers, lose gatekeeping authority over marriage
 *   - Women in traditionalist communities: paradoxical position — gain civil rights but lose community legitimacy
 *   - Interfaith couples: beneficiaries, can marry across religious lines under state law
 *   - Religious authorities: excluded from adjudication, lose binding force on marriage validity
 *   - Civil courts: enforce the secular regime and resolve disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.38).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.22).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract under State Law (Secular Contractual Reading)").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '4c8d3257-77bd-4862-863d-82d2c4cc3efa').
narrative_ontology:cs_kernel_codification('4c8d3257-77bd-4862-863d-82d2c4cc3efa', formalized).
narrative_ontology:cs_authority_grounding('4c8d3257-77bd-4862-863d-82d2c4cc3efa', extraction).
narrative_ontology:cs_interpretation_layer_present('4c8d3257-77bd-4862-863d-82d2c4cc3efa').
narrative_ontology:cs_reading_relation('4c8d3257-77bd-4862-863d-82d2c4cc3efa', family_law_authority__christian_canonical_reading, forecloses).
narrative_ontology:cs_reading_relation('4c8d3257-77bd-4862-863d-82d2c4cc3efa', family_law_authority__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('4c8d3257-77bd-4862-863d-82d2c4cc3efa', family_law_authority__hindu_dharmashastra_reading, influences).
narrative_ontology:cs_reading_relation('4c8d3257-77bd-4862-863d-82d2c4cc3efa', family_law_authority__parsi_zoroastrian_reading, influences).
narrative_ontology:cs_axiom('4c8d3257-77bd-4862-863d-82d2c4cc3efa', foundational, autonomous_individual_consent_grounds_marriage).
narrative_ontology:cs_axiom_status(autonomous_individual_consent_grounds_marriage, holdable).
narrative_ontology:cs_axiom_grounding('4c8d3257-77bd-4862-863d-82d2c4cc3efa', autonomous_individual_consent_grounds_marriage, deontological).
narrative_ontology:cs_axiom('4c8d3257-77bd-4862-863d-82d2c4cc3efa', foundational, state_law_supersedes_religious_authority_in_family_adjudication).
narrative_ontology:cs_axiom_status(state_law_supersedes_religious_authority_in_family_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('4c8d3257-77bd-4862-863d-82d2c4cc3efa', state_law_supersedes_religious_authority_in_family_adjudication, conventional).
narrative_ontology:cs_axiom('4c8d3257-77bd-4862-863d-82d2c4cc3efa', secondary, gender_symmetric_rights_are_civil_requirement).
narrative_ontology:cs_axiom_status(gender_symmetric_rights_are_civil_requirement, holdable).
narrative_ontology:cs_axiom_grounding('4c8d3257-77bd-4862-863d-82d2c4cc3efa', gender_symmetric_rights_are_civil_requirement, deontological).
narrative_ontology:cs_reference_frame('4c8d3257-77bd-4862-863d-82d2c4cc3efa', secular_legal_authority_over_family_law).
narrative_ontology:cs_drift_state('4c8d3257-77bd-4862-863d-82d2c4cc3efa', contemporary_identity_politics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c8d3257-77bd-4862-863d-82d2c4cc3efa', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_civil_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, gender_egalitarian_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, secular_married_individuals).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, women_in_traditional_communities).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, religious_traditionalist_communities).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, women_in_traditional_communities).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, autonomous_individual_rights).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, separation_of_religious_and_civil_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the terms of valid marriage (age, consent, registration, dissolution), enforces property and inheritance rules, resolves disputes through secular courts, and recognizes marriage as a civil contract between autonomous individuals regardless of religious affiliation. Collects registration fees and administers the civil registry. Enforces gender-symmetric rights in property, custody, and inheritance under this reading.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_civil_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Marry under state law without religious sanction or ceremony required. Gain uniform property rights, inheritance protection, and custody recognition under civil law. Can marry across religious lines without legal impediment. Can exit the marriage by state-recognized divorce without religious authority's consent or involvement.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, secular_married_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Experience the civil law's secular marriage regime as displacing or subordinating their own religious marriage frameworks (Christian sacramental, Islamic nikah, Hindu samskara, Zoroastrian practices). Their marriages are legally valid only when registered with the state, not by virtue of religious sanction alone. Court systems apply secular divorce and property law rather than deferring to internal religious adjudication.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_traditionalist_communities, payer,
    organized, generational, constrained, national).

% Gain state-recognized rights to property, custody, and divorce regardless of religious community practice (many traditional regimes restrict these). Can petition civil courts for property division and child custody without requiring community or religious-leader consent. Yet remain embedded in communities whose internal authority structures may deny recognition to civil outcomes, creating dual jeopardy: civil rights without community legitimacy.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, women_in_traditional_communities, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, women_in_traditional_communities, payer).

% Can marry under state law without religious authority gatekeeping or requiring conversion. Legal recognition is automatic upon registration, independent of religious approval from either partner's community. Gain joint property rights and inheritance without sectarian impediment.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, mobile, national).

% Are systematically excluded from adjudicating marriage validity and dissolution. Their own marriage law frameworks (ecclesiastical courts, qadi courts, Dharma Sabha, fire-temple councils) operate in parallel to civil law but have no binding force on state recognition or property/custody outcomes. Their authority is contingent: recognized only where individuals voluntarily consent to both civil AND religious adjudication.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_authorities, excluded,
    organized, generational, trapped, national).

% Apply uniform family law code to all parties regardless of religion. Adjudicate property disputes, custody contests, and divorce petitions under secular law principles. Operate as the authoritative institution enforcing state authority over family law, including in matters historically governed by religious law.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for gender-symmetric marriage rights and secular civil authority as the vehicle for these rights. Position state law as protective against discriminatory religious law. Support secular marriage registration and civil divorce as safeguards against gender-asymmetric traditional practices.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, gender_egalitarian_advocates, beneficiary,
    powerful, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, state_civil_authority).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, uniform legal framework for marriage recognition, property rights, custody, and dissolution available to all residents of a jurisdiction, independent of religious identity. Solves the problem of adjudicating competing claims on marital property, child custody, and inheritance across a religiously plural population by establishing secular law as the common ground.
% TRANSFER_FUNCTION: Transfers authority over marriage validity, rights, and dissolution from religious institutions to the state. Transfers governance authority over interfaith marriage and gender-symmetric rights from religious authorities (who traditionally restricted or forbade such marriages) to civil authorities. Redistributes authority to adjudicate custody and property from family/community councils to state courts.
% ABSENT_VOICES: Religious authority holders (Christian clergy, Islamic judges, Hindu scholars, Zoroastrian priests) would object if seated: they would argue their frameworks offer deeper legitimacy and community coherence than state registration. They would resist the subordination of their marriage law to secular adjudication and the automatic recognition of interfaith marriages their traditions restrict.
% DISAPPEARANCE_RATIONALE: If state-registered civil marriage disappeared and authority reverted wholly to religious law, millions of marriages in religiously mixed jurisdictions would lose legal status; property rights, custody, and inheritance would fall under multiple competing authorities; gender-symmetric rights in many jurisdictions would collapse under traditionally-asymmetric religious law; interfaith couples in restrictive jurisdictions would lose legal recognition entirely. The social organization of property, custody, and succession would reorganize around religious communities, not state jurisdiction.
% FOUNDING_PROBLEM: Early modern religious pluralism: as populations became religiously mixed (through migration, conquest, inter-marriage), no single religious authority could adjudicate family law for all. Property and custody disputes crossed religious lines; inheritance claims multiplied across sectarian boundaries; individuals faced legal uncertainty and conflicting authority claims when religious traditions differed.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary multireligious states (India, Canada, Australia, EU member states) and historical records from the Enlightenment transition document the founding problem: conflicting religious authorities producing unresolvable disputes. Secular legal scholars and pluralist advocacy organizations outside the state apparatus attest the problem persists; only the constitutional separation of civil and religious authority resolves it. Religious authority holders contest this diagnosis, arguing the problem arises from the state's intrusion, not from pluralism itself.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint gathers real authority into state hands: civil registration becomes the sole validity criterion, replacing or subordinating religious ceremonies; civil courts override religious adjudication; secular property and custody law supersedes traditional rules. Yet the extraction is constrained by three factors: (1) individuals can still marry religiously in addition to civilly — dual-track legitimacy is permitted; (2) the state's authority traces to a genuine coordination problem (pluralism requires a common ground); (3) the constraint operates mostly on religious communities and religious authorities, not diffusely on the population. Suppression is low (0.22) because the state does not typically use coercive force to compel civil marriage — registration is incentivized through property/custody recognition, not threats. Theater is very low (0.12) because the constraint's function (adjudicating family law across religions) is transparently its real function; there is minimal performative overlay. Accessibility collapse is moderate (0.45) because alternatives exist in principle — traditional religious marriage remains an option for those embedded in communities — but the state's control of property and custody recognition makes civil registration nearly mandatory in practice for anyone needing secular legal recourse. Resistance is substantial (0.58) because religious traditionalists mount ongoing resistance: they maintain parallel authority structures, advocate for religious law recognition, contest civil court rulings on religious grounds, and mobilize to protect gender-asymmetric inheritance or custody rules grounded in their traditions. The measurement series track the constraint's stability: extractiveness rises slightly from 0.28 to 0.40 over t=0 to t=30 as state authority consolidates, then dips slightly to 0.38 by t=40, suggesting either a stabilization or the beginning of a retrenchment phase following heightened religious mobilization.
 *
 * PERSPECTIVAL GAP:
 *   The state civil authority and gender-egalitarian advocates experience the constraint as genuine coordination with equity benefits: it solves the pluralism problem and protects women and minorities from discriminatory traditional law. Religious traditionalists experience it as pure extraction: their authority is subordinated, their frameworks are delegitimized, and their control over marital formation and dissolution is stripped away. Women in traditionalist communities face a dual loss: they are embedded in communities that deny legitimacy to civil outcomes (identity_locked exit), yet they gain civil rights that the community rejects, creating irreconcilable tension. The engine computes these divergences from the power atoms and exit options: state authority and secular individuals have high exit options (arbitrage, mobile) and are institutional/powerful; traditionalist communities have lower institutional power in secular states and constrained exit (exit means leaving the community); women in traditionalist communities are powerless with identity_locked exit. The constraint computes differently from each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The state and gender-egalitarian advocates are beneficiaries: they collect the consolidation of authority, the implementation of secular gender equity, and the vindication of autonomous-individual principles. Their directionality (d) is low, approaching 0.2–0.3 (beneficiaries). Religious traditionalists are payers: they lose gatekeeping authority, face subordination of their frameworks, and must navigate a secular legal regime that may conflict with their principles. Their directionality is higher, around 0.65–0.75 (targets). Women in traditionalist communities are paradoxical: they gain civil rights (d trending toward beneficiary) but remain embedded in communities that deny recognition to those rights (identity_locked exit keeps them trapped, raising d back toward target). This paradox warrants a directionality override for this agent: the structural derivation would place them near 0.55 (symmetric); the true directionality is higher because the identity-lock prevents meaningful exit even where civil rights exist. Override to 0.68 captures the trap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: religious pluralism remains an ongoing condition in contemporary secular states. The constraint's mandate — providing a single legal framework for family law across religions — persists because the problem persists. However, there is tension between the mandate and the implementation: the constraint claims to coordinate across pluralism but also subordinates religious authorities, which may look more like extraction than coordination from a traditionalist perspective. The omega on reading containment addresses this: if the constraint were framed purely as coordination without subordination (e.g., secular law coexisting with full religious authority rather than subordinating it), the extraction would drop and the coordination function would strengthen. The measured extraction (0.38) reflects the real subordination of religious authority; this is not theater or mandate decay but a structural feature of how this reading implements the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_subordination_vs_coordination,
    'Is the subordination of religious authority to state law a necessary cost of pluralist coordination, or an extraction mechanism riding on the coordination function?',
    'Comparative analysis of jurisdictions with genuinely dual-track authority (both religious and civil have binding force in separate domains) versus those with hierarchical subordination (religious authority operates only where civil law defers). If pluralism is stable in dual-track systems without full subordination, the subordination is extraction. If pluralism collapses without subordination, it is coordination cost.',
    'If subordination is extraction, the constraint reclassifies from rope to tangled_rope. If it is coordination cost, rope classification holds. The distinction changes the extraction remedies: coordination cost is legitimate; extraction warrants remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_subordination_vs_coordination, conceptual, 'Whether religious authority subordination is necessary or extractive').

omega_variable(
    identity_lock_suppression_mechanism,
    'For women in traditionalist communities who gain civil rights but lose community legitimacy, is their continued community embeddedness structural (economic dependency, geographic isolation) or internalized (identity fusion, belief in community authority)?',
    'Post-exit trajectories: if women who exit their communities and gain independence also shed the constraints of community authority, the suppression is structural. If suppression persists after physical exit, it is internalized.',
    'Structural suppression means the constraint exerts power through material conditions and can be addressed via exit support. Internalized suppression means the constraint''s power persists through identity fusion and requires different remedies (consciousness-raising, counter-narrative, identity reformation). If internalized, the effective suppression is higher than the measured 0.22.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Whether suppression of traditional community members is structural or internalized').

omega_variable(
    dual_track_legitimacy_sustainability,
    'Can individuals maintain genuine dual legitimacy in both civil and religious marriage frameworks, or does subordination of religious authority eventually erode religious marriage practice?',
    'Time-series data on religious marriage practice in secular jurisdictions: does dual-track marriage persist or decline over generations? Do younger cohorts in traditionalist communities maintain religious marriage ceremonies as civil marriage becomes normalized?',
    'If dual track erodes, religious authority subordination eventually eliminates the sibling reading (Christian sacramental, Islamic nikah, etc.) as live options in secular jurisdictions. The constraint transitions from coexisting to foreclosing the religious readings. If dual track persists, the coexistence relation holds and the religious readings remain live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_track_legitimacy_sustainability, empirical, 'Whether dual-track family law legitimacy sustains over time or collapses').

omega_variable(
    gender_equity_vs_cultural_autonomy_tension,
    'When gender-symmetric civil rights conflict with community cultural norms (e.g., women''s inheritance rights under state law vs. patrilineal custom), does the constraint''s implementation of gender equity override cultural autonomy or do both principles coexist?',
    'Examine court rulings and enforcement: do civil courts enforce gender-symmetric rights even when communities reject them? Are religious communities allowed to opt out of gender-symmetric rights on cultural grounds?',
    'If civil courts override community cultural norms to enforce gender equity, the constraint carries a cultural supremacy component on top of its pluralism coordination function — extractiveness increases, potentially reclassifying from rope to tangled_rope. If communities are allowed cultural opt-out, the constraint is weaker and more truly coordinating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equity_vs_cultural_autonomy_tension, empirical, 'Whether gender-equity implementation overrides or accommodates cultural autonomy').

omega_variable(
    sibling_reading_foreclosure_via_subordination,
    'Does the secular contractual reading''s institutional dominance over time foreclose the sibling religious readings as live alternatives in practice, even if they legally coexist?',
    'Generational cohort analysis: do later cohorts in religious communities who grew up under secular law treat religious readings as non-options, even if available? Do religious authorities themselves transition to treating religious marriage as ceremonial supplement rather than substantive alternative?',
    'If secular authority foreclosure erodes the sibling readings'' perceived legitimacy, the reading relations should be updated from coexists_with to influences or forecloses. This shifts the kernel''s structure: the dispute becomes inactive and the secular reading becomes de facto dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_via_subordination, empirical, 'Whether secular dominance forecloses religious readings as live alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__secular_contractual_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(fami_tr_t5, observed).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__secular_contractual_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(fami_tr_t10, observed).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__secular_contractual_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(fami_tr_t20, observed).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__secular_contractual_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(fami_tr_t30, observed).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__secular_contractual_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(fami_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t5, family_law_authority__secular_contractual_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(fami_be_t5, observed).
narrative_ontology:measurement(fami_be_t10, family_law_authority__secular_contractual_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(fami_be_t10, observed).
narrative_ontology:measurement(fami_be_t20, family_law_authority__secular_contractual_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(fami_be_t20, observed).
narrative_ontology:measurement(fami_be_t30, family_law_authority__secular_contractual_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(fami_be_t30, observed).
narrative_ontology:measurement(fami_be_t40, family_law_authority__secular_contractual_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(fami_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t5, family_law_authority__secular_contractual_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement_basis(fami_su_t5, observed).
narrative_ontology:measurement(fami_su_t10, family_law_authority__secular_contractual_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(fami_su_t10, observed).
narrative_ontology:measurement(fami_su_t20, family_law_authority__secular_contractual_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement_basis(fami_su_t20, observed).
narrative_ontology:measurement(fami_su_t30, family_law_authority__secular_contractual_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement_basis(fami_su_t30, observed).
narrative_ontology:measurement(fami_su_t40, family_law_authority__secular_contractual_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(fami_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.18).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the family_law_authority kernel. The secular contractual reading subordinates religious authority to state law and grounds marriage in autonomous individual consent. It coexists with religious readings in pluralist jurisdictions but influences and potentially forecloses them over time through institutional dominance. All five stories are linked via affects_constraints; each carries its own ε, stakeholder structure, and beneficiary/victim declarations. The kernel contest is not resolved here — each reading instantiates the authority question from its own framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__secular_contractual_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
