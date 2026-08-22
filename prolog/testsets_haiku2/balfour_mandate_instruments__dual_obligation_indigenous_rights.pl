% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate Dual Obligation to Indigenous Rights and Land Tenure
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The Balfour Mandate system imposed on the British administration of
 *   Palestine a dual obligation: (1) protection of existing Arab civil and
 *   political rights and land tenure (the 'non-Jewish communities' clause),
 *   and (2) facilitation of a Jewish 'national home.' This constraint story
 *   instantiates the reading that subordinates the national-home language to
 *   indigenous-rights and self-determination norms. Under this reading, the
 *   Mandate covenant's language on 'sacred trust' and minority protection
 *   establishes binding obligations superior to the national-home commitment.
 *   Land transfer restrictions, immigration caps tied to economic absorptive
 *   capacity, and Arab majority representation in governance are the
 *   enforcement mechanisms. This reading was articulated by Palestinian Arab
 *   nationalists, some international law scholars, and League advisory
 *   bodies. It was systematically challenged and eventually overridden in
 *   British administrative practice by the competing
 *   jewish_national_home_primacy reading. The constraint story models what
 *   the law said under this reading, not what British authorities ultimately
 *   did.
 *
 * KEY AGENTS:
 *   - Palestinian Arab communities: indigenous majority population with established tenure; trapped beneficiaries of the dual obligation if enforced
 *   - Palestinian Arab political elites: moderate-power articulate advocates for this reading; agenda-setters in the beneficiary coalition
 *   - Zionist organizations: organized transnational actors experiencing the constraint as land and immigration restrictions; active payers seeking to override it
 *   - British mandatory administration: institutional power with formal obligations under the Mandate; payers experiencing the constraint as limitation on administrative discretion
 *   - League of Nations: observer authority with nominal oversight and enforcement power; practically limited in capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate Dual Obligation to Indigenous Rights and Land Tenure").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'c4290118-1331-4004-a070-acf449d2e583').
narrative_ontology:cs_kernel_codification('c4290118-1331-4004-a070-acf449d2e583', fixed_text).
narrative_ontology:cs_authority_grounding('c4290118-1331-4004-a070-acf449d2e583', lineage).
narrative_ontology:cs_interpretation_layer_present('c4290118-1331-4004-a070-acf449d2e583').
narrative_ontology:cs_reading_relation('c4290118-1331-4004-a070-acf449d2e583', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('c4290118-1331-4004-a070-acf449d2e583', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('c4290118-1331-4004-a070-acf449d2e583', foundational, existing_arab_rights_superior_to_national_home).
narrative_ontology:cs_axiom_status(existing_arab_rights_superior_to_national_home, holdable).
narrative_ontology:cs_axiom_grounding('c4290118-1331-4004-a070-acf449d2e583', existing_arab_rights_superior_to_national_home, deontological).
narrative_ontology:cs_axiom('c4290118-1331-4004-a070-acf449d2e583', foundational, demographic_majority_grounds_governance_authority).
narrative_ontology:cs_axiom_status(demographic_majority_grounds_governance_authority, holdable).
narrative_ontology:cs_axiom_grounding('c4290118-1331-4004-a070-acf449d2e583', demographic_majority_grounds_governance_authority, conventional).
narrative_ontology:cs_axiom('c4290118-1331-4004-a070-acf449d2e583', secondary, land_tenure_protection_binding_obligation).
narrative_ontology:cs_axiom_status(land_tenure_protection_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c4290118-1331-4004-a070-acf449d2e583', land_tenure_protection_binding_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('c4290118-1331-4004-a070-acf449d2e583', mandate_covenant_sacred_trust_indigenous_protection).
narrative_ontology:cs_drift_state('c4290118-1331-4004-a070-acf449d2e583', mandate_end_1948, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c4290118-1331-4004-a070-acf449d2e583', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_political_elites).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Indigenous Arab population with established land tenure, civil administrative structures, and cultural institutions dating centuries prior to the Mandate. This reading's constraint protects their majority demographic status, prevents forced land dispossession, restricts immigration to prevent replacement, and grounds their claim to representative government and eventual self-determination. They cannot exit—this is their ancestral territory. They benefit from the dual obligation framing if enforced.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    powerless, generational, trapped, regional).

% Arab nationalist leadership, landowning classes, and emerging professional elites who advocate for the dual-obligation reading. They articulate the claim that Mandate instruments subordinate the 'national home' to minority-protection and self-determination norms. They stand to inherit governance authority if this reading is enforced. Their exit option is political emigration, but leadership identity is constituted through this territorial claim and nationalist mobilization.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_political_elites, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_political_elites, agenda_setter).

% Transnational Jewish organizations pursuing territorial acquisition, immigration facilitation, and institutional establishment to build a Jewish polity. Under this reading, they pay through land transfer restrictions, capped immigration quotas, and subordination of the 'national home' language to existing Arab rights. Their exit is geographic—they can pursue settlement elsewhere—but institutional commitment to Palestine specifically is high and ideologically constituted. They experience this constraint as blocking their stated objectives.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, constrained, global).

% The British Mandate Power, charged by the League of Nations with administering Palestine under a sacred trust. This reading imposes the obligation to enforce land-tenure protections, quota caps on immigration, and advancement of Arab representative government—obligations that conflict with concurrent pressure from Zionist organizations and with British strategic interests in Jewish organizational cooperation. They can exit by ending the Mandate, but international law and the covenant formally constrain their discretion. They experience this reading as a constraint on their administrative freedom.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, payer,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, agenda_setter).

% International body authorizing the Mandate and articulating the legal framework. Theoretically responsible for reviewing compliance with minority-protection and self-determination principles. In practice, they have limited enforcement capacity and Britain exercises interpretive discretion. They provide the formal authority context but do not actively adjudicate between readings.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations, observer,
    institutional, generational, analytical, universal).

% The body of doctrine on mandates, self-determination, minority protection, and colonial administration. Exists as a normative reference frame; does not itself act.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, international_law_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(balfour_mandate_instruments__dual_obligation_indigenous_rights, international_law_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Implements international oversight of colonial administration through minority protection and self-determination principles: the Mandate system was designed to reconcile European imperial interests with indigenous rights through League authority and accountability mechanisms.
% TRANSFER_FUNCTION: Transfers political authority, land tenure, and demographic control FROM Zionist organizations and British discretionary power TO Palestinian Arab majorities through enforcement of property protections, immigration caps, and self-governance advancement. The constraint redistributes power by restricting land markets and population flows.
% ABSENT_VOICES: Rival interpretations of the Mandate held by Zionist organizations and British mandatory authorities are excluded from this reading's adjudication. They would argue the 'national home' takes precedence over existing-rights protection and that British discretion permits demographic transformation. The legal interpretation itself is contested—no party holds the truth; this reading is ONE party's claim about what the law requires.
% DISAPPEARANCE_RATIONALE: If this reading's dual-obligation constraint were removed and replaced with the jewish_national_home_primacy reading, the political trajectory would reorganize fundamentally: land markets would open to Jewish acquisition, immigration caps would rise or vanish, Arab representative institutions would be subordinated to Jewish institutional development, and demographic change would accelerate. The territorial and political outcome would be structurally different.
% FOUNDING_PROBLEM: The League of Nations Mandate system was constructed to resolve the tension between British imperial commitments (the Balfour Declaration's support for a Jewish 'national home') and the rights of the indigenous Arab majority already settled in Palestine. The founding problem: how to honor the national-home commitment while protecting indigenous populations from displacement and ensuring they retained political voice commensurate with their majority status.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars and historians outside the benefiting parties (Arabs alone) attest that the Mandate covenant explicitly imposed obligations to protect existing rights and advance self-determination. The League Covenant, Article 22, grounded mandates in a 'sacred trust' and subordinated the administering power's discretion to minority protection. Arab nationalist scholars and Palestinian political elites articulate this reading. Zionist organizations and British authorities contest it, claiming interpretive discretion or primacy of the national-home language. International law doctrine splits: minority-protection and self-determination readings align with this constraint; discretionary-power and national-home-primacy readings do not.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 by 1948) because the constraint redistributes political and territorial control away from Zionist organizations toward Arab majorities through property protections and majority-rule governance. It rises over the Mandate period (from 0.35 to 0.68) as British practice increasingly diverges from this reading's enforcement, making the gap between the obligation and its non-implementation itself a measure of extraction—the constraint is increasingly breached. Suppression is high (0.72 by 1948) because maintaining this reading against competing interpretations requires active enforcement: land transactions must be reviewed, immigration quotas monitored, Arab representation protected against institutional marginalization. Theater ratio climbs from 0.18 to 0.41, indicating increasing performative activity (official League and British rhetoric affirming minority protection) while practical enforcement erodes. The measurement grid shares one time axis (interval 1920–1948, shared endpoints) across all three metrics, authored at six points. Early values (1920) are projected; later values (1926–1948) rest on observed historical records of land policy, immigration, and administrative practice.
 *
 * PERSPECTIVAL GAP:
 *   The Arab beneficiary seats (Palestinian Arab communities and elites) would experience this constraint as exactly what the law requires: protection of their demographic majority, land tenure security, and path to self-governance. The payer seats (Zionist organizations and British administration) experience it as an obstacle: British administrators face pressure from Zionist organizations to facilitate Jewish land acquisition and immigration contrary to this reading; Zionist organizations face the constraint as a barrier to territorial expansion and demographic transformation. The engine should compute divergent types from the structural data: from the Arab seat, a genuine tangled_rope reconciling coordination (League oversight of colonial administration) with asymmetric extraction (of Zionist institutional ambitions); from the British seat, a snare or tangled_rope depending on how trapped they are in competing mandates; from the Zionist seat, a snare—the constraint is pure extraction with no coordination benefit to them. The authored claim (tangled_rope) reflects the global structure; the per-seat computations will diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab communities and elites are beneficiaries (d near 0.0–0.2): they collect protection of rights, tenure security, and governance advancement. Their exit is trapped (ancestral territory, identity-constituted political claim), which modulates d toward lower values (less exit = more beneficiary-like subsidy). Zionist organizations are victims (d near 0.8–1.0): they pay through land and immigration restrictions; their exit is constrained (commitment to Palestine as the destination is ideological and organizational identity, not freely chosen), which modulates d upward (trapped-exit targets are more extractive). British mandatory administration is a victim (d around 0.6–0.75): they pay through obligation enforcement against competing pressure, but they hold institutional power and mobile exit (they can end the Mandate—they eventually did), which modulates d downward from pure-target levels. The directionality should reflect that power differentiates exit: powerless Arabs with trapped exit get low d (subsidy); organized Zionists with constrained exit get high d (extraction); institutional Britain with mobile exit gets moderate-high d (constrained but not purely targeted).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic mandatrophy: the founding problem (reconciling the Balfour commitment with indigenous rights) remains live and contested throughout the Mandate period, but the constraint itself becomes increasingly theatrical. Theater ratio rises from 0.18 to 0.41 over 28 years. League bodies affirm minority-protection obligations; British administrators issue land-transfer regulations and immigration reviews; Arab institutions are expanded. Yet practical enforcement erodes: Jewish land acquisition continues through circumvention and administrative discretion; immigration quotas are repeatedly exceeded; Arab representative institutions are subordinated. The constraint persists nominally (in the Mandate text and League rhetoric) but operates as performance—the machinery of enforcement is maintained, but its output diverges from its stated function. This is precisely the piton signature: the founding obligation remains unkilled, but the system delivers extraction (Zionist demographic and territorial expansion) while performing the obligation's opposite. The measurement series capture this by showing theater rising while suppression and extractiveness also rise—the system is increasingly doing two things: performatively affirming rights protection while operationally enabling demographic displacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competing_readings_kernel_contest,
    'Which reading of the Mandate kernel is legally correct: dual_obligation_indigenous_rights, jewish_national_home_primacy, or mandatory_interpretive_discretion?',
    'International Court of Justice advisory opinion on League Mandate interpretation (never sought). Historical-textual analysis: comparative reading of Mandate text, League Covenant Article 22, Balfour Declaration, and contemporary international law doctrine on mandates and self-determination. Party-neutral scholars external to all beneficiary coalitions.',
    'High. The correct reading determines whether the constraint is properly classified as tangled_rope (dual obligation), whether extractiveness should be re-evaluated, whether the Zionist organizations and British administration should compute as victims or as obstacle-overcomers, and whether the constraint should be reclassified to snare (national-home-primacy reading) or piton (discretion reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_readings_kernel_contest, conceptual, 'The kernel admits multiple structurally coherent readings; the readings generate different constraint types and directionalities.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (legal barriers, administrative review, international authority) or internalized (Arab acceptance of Mandate legitimacy, British internalization of competing obligations)?',
    'Post-Mandate trajectory analysis: (1) Did suppression persist after the constraint disappeared (1948–1967)? If Arabs resisted Zionist authority, suppression was structural; if they accepted, partly internalized. (2) What mechanisms of suppression persisted? De facto customs, legal codes inherited from Mandate, or active new enforcement? Structural suppression leaves enforcement machinery; internalized leaves beliefs.',
    'If internalized, the constraint''s effective suppression exceeded the structural measure—the target population carried the suppression mentality forward. If structural, the measured 0.72 reflects observable enforcement machinery accurately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression was maintained by external barriers (structural) or absorbed into the beliefs and identities of suppressed parties (internalized).').

omega_variable(
    mandate_clause_hierarchy,
    'Does the Mandate instrument hierarchically order its clauses (dual obligation, with minority protection superior to national-home facilitation), or are the clauses equipotent, leaving hierarchy to British administrative discretion?',
    'Textual analysis of Mandate language, League Covenant, and preparatory documents. Comparison to other Mandate instruments (Syria, Iraq, Cameroon) to establish whether hierarchy was standard or Palestine-specific. Historical testimony from League officials and legal advisors about clause-ordering intent.',
    'If hierarchically ordered (this reading''s assumption), the dual obligation is binding and Britain''s deviation constitutes Mandate breach. If equipotent, the mandatory_interpretive_discretion reading gains support and Britain''s latitude expands. The classification hinges on this: hierarchy → tangled_rope; discretion → piton (administrative choice maintained theatrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_clause_hierarchy, empirical, 'Whether the Mandate''s clauses were ordered hierarchically or left to discretionary interpretation.').

omega_variable(
    zionist_organizational_exit_framing,
    'Were Zionist organizations truly trapped by this constraint (constrained exit modulation → high d), or did they retain arbitrage options (geographic relocation to other settlement sites, political alternatives)?',
    'Historical analysis of Zionist organizational debate: did they discuss and pursue alternatives to Palestine (Uganda scheme, Argentina, Madagascar, Biro Bidjan)? What kept commitment to Palestine despite the constraint? Was it territorial primacy, religious/historical identity, or strategic calculation? Would removal of the constraint have redirected settlement elsewhere, or was Palestine the only viable option?',
    'High exit (arbitrage) would modulate d toward beneficiary levels (~0.2–0.4), reclassifying Zionist organizations as less victimized and more strategically choosing constraint-breaking. Trapped exit would support the authoring d (~0.8), maintaining snare-classification for Zionist experience. The directionality divergence hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zionist_organizational_exit_framing, empirical, 'Whether Zionist organizations experienced trapped or constrained exit, affecting their directionality value.').

omega_variable(
    british_discretion_authority_actual_scope,
    'Did the Mandate covenant actually grant Britain broad interpretive discretion over the competing clauses, or did it impose binding minority-protection obligations that constrained discretion to the margins?',
    'League legal opinions and advisory authority positions. Comparative analysis of Mandate instruments: how did other mandatory powers (France in Syria, Belgium in Congo) interpret their obligations? Did League bodies challenge British interpretations as discretionary or uphold them as within League authority? Post-Mandate review: did League bodies, in retrospect, classify Britain''s administration as compliant or breaching?',
    'If Britain held broad discretion, the mandatory_interpretive_discretion reading is correct, and this constraint (dual_obligation_indigenous_rights) is a claim about what law should say, not what law actually enforced. If discretion was constrained by binding obligations, this reading describes law that should have been enforced but wasn''t—the constraint is real but breached, supporting the piton classification (obligation maintained but not executed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_discretion_authority_actual_scope, conceptual, 'Whether Britain''s interpretive authority was constrained (binding obligations) or discretionary (law-making in practice).').

omega_variable(
    arab_demographic_majority_political_weight,
    'Did Arab demographic majority actually ground a political claim to representative government under the Mandate system, or was the demographic fact decoupled from governance authority?',
    'Analysis of Mandate-era governance institutions: did Arab majorities translate to Arab majority in the legislative council, executive appointments, property and commerce regulation? What were the stated justifications for any governance gaps (trusteeship, capacity, security)? Post-Mandate comparisons: did other Mandate territories with indigenous majorities achieve representative government prior to independence, or was Palestine exceptional in decoupling demography from governance?',
    'If demographic majority grounded governance authority, the constraint''s enforceability is higher and the dual obligation is meaningful. If demographics were decoupled from governance (British and Zionist institutional dominance despite Arab majority), the constraint was never actually enforced and the extractiveness valuation may be understated (the constraint was systematically violated from inception).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_demographic_majority_political_weight, empirical, 'Whether Arab demographic majority translated into political representation and governance authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_dual_obligation_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1926, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1926, 0.24).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1932, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1932, 0.32).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1938, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1938, 0.38).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1944, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1944, 0.4).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.41).

% Extraction over time
narrative_ontology:measurement(balfour_dual_obligation_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(balfour_dual_obligation_be_t1926, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1926, 0.48).
narrative_ontology:measurement(balfour_dual_obligation_be_t1932, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1932, 0.58).
narrative_ontology:measurement(balfour_dual_obligation_be_t1938, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1938, 0.64).
narrative_ontology:measurement(balfour_dual_obligation_be_t1944, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1944, 0.68).
narrative_ontology:measurement(balfour_dual_obligation_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balfour_dual_obligation_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement(balfour_dual_obligation_su_t1926, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1926, 0.55).
narrative_ontology:measurement(balfour_dual_obligation_su_t1932, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1932, 0.64).
narrative_ontology:measurement(balfour_dual_obligation_su_t1938, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1938, 0.69).
narrative_ontology:measurement(balfour_dual_obligation_su_t1944, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1944, 0.71).
narrative_ontology:measurement(balfour_dual_obligation_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_minority_protection_system).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandate_system_self_determination_clause).

% DUAL FORMULATION NOTE:
% The Balfour Mandate system is a single kernel with three structurally distinct readings: (1) dual_obligation_indigenous_rights (this file) prioritizes minority protection and self-determination, subordinates national home; (2) jewish_national_home_primacy interprets national home as proto-state requiring facilitation; (3) mandatory_interpretive_discretion treats British authority as paramount. The three readings are coexistent positions held by different parties. Each reading generates a different constraint with different epsilon values, beneficiary/victim structures, and classifications. This story models the dual-obligation reading's structural logic; it is neither the only nor the ultimately-enforced reading, but it is a live position in international law doctrine and Palestinian Arab political claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, powerless, 0.15).
constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, organized, 0.82).
constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
