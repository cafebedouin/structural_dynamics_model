% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Framework for Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The diasporist reading of Jewish self-determination asserts that Jewish
 *   collective survival is best secured through diaspora pluralism, minority
 *   rights, and cultural autonomy — not territorial sovereignty. It views
 *   Zionism as a dangerous deviation that ties Jewish fate to a militarized
 *   state, makes Jews complicit in Palestinian dispossession, and fuels
 *   antisemitism by identifying all Jews with Israeli actions. This reading
 *   was once a live coordination framework (the Bund, cultural autonomy
 *   movements, pre-1948 cultural Zionism) but has atrophied into a piton
 *   under Zionist hegemony: diaspora institutions are weakened, the framework
 *   persists mostly through performative maintenance by anti-Zionist Jewish
 *   organizations, and its coordination function is severely degraded. The
 *   constraint story models this atrophied framework as a piton — a former
 *   rope whose primary function has decayed but whose residual structure
 *   still shapes a niche of Jewish life.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.52).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.35).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Framework for Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, 'ff14922e-c55b-4166-aca6-87bba9d59845').
narrative_ontology:cs_kernel_codification('ff14922e-c55b-4166-aca6-87bba9d59845', distributed).
narrative_ontology:cs_authority_grounding('ff14922e-c55b-4166-aca6-87bba9d59845', distributed).
narrative_ontology:cs_reading_relation('ff14922e-c55b-4166-aca6-87bba9d59845', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff14922e-c55b-4166-aca6-87bba9d59845', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff14922e-c55b-4166-aca6-87bba9d59845', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff14922e-c55b-4166-aca6-87bba9d59845', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('ff14922e-c55b-4166-aca6-87bba9d59845', foundational, diaspora_pluralism_secures_survival).
narrative_ontology:cs_axiom_status(diaspora_pluralism_secures_survival, holdable).
narrative_ontology:cs_axiom_grounding('ff14922e-c55b-4166-aca6-87bba9d59845', diaspora_pluralism_secures_survival, instrumental).
narrative_ontology:cs_axiom('ff14922e-c55b-4166-aca6-87bba9d59845', foundational, zionism_endangers_jewish_fate).
narrative_ontology:cs_axiom_status(zionism_endangers_jewish_fate, holdable).
narrative_ontology:cs_axiom_grounding('ff14922e-c55b-4166-aca6-87bba9d59845', zionism_endangers_jewish_fate, empirically_contingent).
narrative_ontology:cs_reference_frame('ff14922e-c55b-4166-aca6-87bba9d59845', diasporist_jewish_autonomy).
narrative_ontology:cs_drift_state('ff14922e-c55b-4166-aca6-87bba9d59845', post_1948_zionist_hegemony, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ff14922e-c55b-4166-aca6-87bba9d59845', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, anti_zionist_jewish_organizations).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_association).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, host_state_minorities).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, minority_rights_pluralism).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, non_territorial_national_autonomy).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, jewish_survival_without_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain Jewish distinctiveness through cultural institutions, Yiddish/Hebrew diaspora culture, minority rights advocacy, and integration without assimilation. Benefit from a framework that legitimizes Jewish life outside Israel. Their exit from the diasporist framework would mean either aliyah (accepting Zionist terms) or full assimilation (loss of distinct identity); both are structurally constrained by host-state conditions and internal community pressures.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Organizations (Jewish Voice for Peace, IfNotNow, Independent Jewish Voices, Jewish Currents, T'ruah) that articulate and maintain the diasporist framework. They produce counter-narratives to Zionist hegemony, provide institutional homes for dissident Jews, and advocate minority rights frameworks. Their identity is fused with this position — exit means losing organizational purpose and communal belonging. They collect status and funding from the niche they occupy but lack power to challenge Zionist institutional dominance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, anti_zionist_jewish_organizations, agenda_setter,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, anti_zionist_jewish_organizations, beneficiary).

% Jews who experience communal pressure to affirm Zionism as constitutive of Jewish identity — through 'stand with Israel' litmus tests, Birthright trips, synagogue Israel programming, and accusations of 'self-hatred' or 'antisemitism' for dissent. They bear the psychological and social costs of either conforming or facing ostracism. The weakness of the diasporist alternative (piton) means no viable communal home exists for their dissent, forcing a choice between self-betrayal and exile from communal life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework, payer,
    moderate, biographical, identity_locked, global).

% Jews worldwide who face rising antisemitism fueled by identification of all Jews with Israeli state actions — especially during Gaza wars, settlement expansion, and right-wing Israeli governance. They bear physical and social risks (attacks, harassment, professional retaliation) for actions they do not control and often oppose. The Zionist claim to represent all Jews makes them human shields for Israeli policy; the atrophied diasporist framework cannot effectively counter this representation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_association, payer,
    powerless, immediate, trapped, global).

% Major Jewish federations, ADL, AIPAC, Conference of Presidents, Israeli government diaspora affairs ministry, and mainstream denominational bodies that define 'Jewish interest' as synonymous with Zionism. They control communal resources, define communal boundaries, and enforce the Zionist consensus. They are excluded from the diasporist constraint's internal logic but structurally dominate the field in which it operates — their suppression is the primary reason the diasporist framework is a piton rather than a rope.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutions, excluded,
    institutional, generational, arbitrage, global).

% Other minority communities (Muslim, Roma, Black, Indigenous, immigrant) in diaspora host states who benefit from the diasporist framework's alignment with minority rights, pluralism, and anti-racist coalitions. The Jewish diasporist tradition provides historical precedent and institutional capacity for minority self-advocacy without territorial nationalism. Their exit from this alignment is constrained by the structural need for coalition partners.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_state_minorities, beneficiary,
    organized, generational, constrained, national).

% Scholars of Jewish history, nationalism studies, postcolonial theory, and political philosophy who analyze the diasporist framework as a live intellectual tradition and a structural case study in nationalist hegemony and counter-tradition. They neither collect rents nor bear costs from the constraint's operation but their work shapes the discursive field in which it survives or atrophies.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Diaspora pluralism coordinates Jewish collective survival through minority rights frameworks, cultural autonomy, and host-state integration — solving the problem of Jewish continuity without requiring territorial sovereignty or a militarized state. It provides a model for non-territorial national self-determination compatible with liberal democracy and multinational coexistence.
% TRANSFER_FUNCTION: Moves communal resources (funding, institutional capacity, narrative authority) toward diaspora institution-building, Yiddish/Hebrew cultural production, minority rights advocacy, and anti-racist coalition work. Moves Jewish identity formation away from territorial nationalism and toward ethical universalism rooted in particularist history. The transfer is currently weak because Zionist institutions capture the bulk of communal resources.
% ABSENT_VOICES: Palestinians — though this reading centers Jewish survival, the Palestinian experience of dispossession is the shadow that makes the diasporist critique of Zionism morally urgent. Assimilated Jews who reject any distinct Jewish collective identity — they would deny the premise that Jewish survival requires any framework at all. Mizrahi and Sephardi Jews whose historical experience of coexistence in Muslim-majority societies complicates both the Zionist 'return' narrative and the Ashkenazi-centric diasporist narrative of European persecution.
% DISAPPEARANCE_RATIONALE: If the diasporist framework vanished entirely, Jewish identity would be fully captured by the Zionist framework — no organized alternative would exist to contest the equation 'Jewish = Zionist.' This would accelerate the endorsement of Israeli state actions as 'Jewish self-defense,' deepen the human-shield dynamic for diaspora Jews, and eliminate the primary Jewish contribution to minority-rights pluralism. Host-state minorities would lose a historic coalition partner. The world would rearrange toward a more monolithic, militarized Jewish politics.
% FOUNDING_PROBLEM: How to ensure Jewish collective survival and flourishing in diaspora after the Enlightenment shattered pre-modern communal autonomy, without resorting to territorial sovereignty that would replicate the nation-state's exclusionary logic. The Bund, Simon Dubnow's national-cultural autonomy, and cultural Zionism (Ahad Ha'am) offered pre-1948 answers; the Holocaust and 1948 made the Zionist answer hegemonic.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by pre-1948 Jewish political history (Bund electoral success in Poland, Dubnow's influence, Ahad Ha'am's cultural Zionism) — sources outside the current diasporist beneficiaries. Contemporary corroboration comes from minority rights scholars (Will Kymlicka, Jane Jenson), postcolonial theorists (Edward Said, Ella Shohat), and the documented survival of non-Zionist Jewish communities (Satmar, Neturei Karta, secular Yiddishists) — though these groups often reject the liberal pluralist framing of contemporary diasporism. No major Zionist institution corroborates the problem's legitimacy; they treat it as solved by statehood.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because maintaining diasporic distinctiveness extracts real costs (vulnerability to host-state persecution, social friction, resource investment in parallel institutions) while the coordination benefit (collective survival) is no longer reliably delivered — the framework's atrophied state means participants pay costs without the full rope's payoff. Suppression is low-moderate (0.35) because the diasporist constraint itself suppresses little; it is the OBJECT of suppression by Zionist institutions. Theater ratio is high (0.55) — residual institutions (YIVO, Workmen's Circle remnants, progressive synagogues, JVP) perform diasporist coordination without the communal authority to make it effective. Accessibility collapse is moderate (0.55) because the Zionist 'Jewish = Zionist' equation makes cognitive exit difficult, but the diasporist alternative remains intellectually and culturally accessible. Resistance is moderate (0.45) — anti-Zionist Jewish organizing has grown since 2014 but remains marginal in communal power terms.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (anti-Zionist orgs), the constraint is a living rope trying to regenerate — they experience genuine coordination among dissident Jews. From the payer seats (coerced/endangered Jews), it is a failed rope that offers no real exit — they experience the framework's atrophied state as abandonment. From the excluded seat (Zionist institutions), it is a negligible irritant — they experience it as a marginal discourse they successfully contain. The engine computes these divergences from the structural data; the claimed_type (piton) reflects the analytical observer's assessment that the framework's primary coordination function has atrophied.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities and anti-Zionist organizations are beneficiaries (d ~ 0.2-0.3) — they collect identity, community, and moral coherence from the framework. Jews coerced into Zionism and Jews endangered by Israeli association are payers (d ~ 0.8-0.9) — they bear the costs of the framework's weakness (no viable exit from Zionist consensus, physical danger from association). Zionist institutions are excluded (d not computed) — they are the external suppressing power, not a seat within this constraint. Host-state minorities are secondary beneficiaries (d ~ 0.3) — they gain a coalition model. The analytical observer sits at d=0.5. Identity_locked exit for coerced Jews and anti-Zionist orgs reflects professional/communal identity fusion: leaving the position means losing one's community and often one's livelihood.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist framework was built to solve the problem of Jewish survival in diaspora without sovereignty (founding_problem). That problem is contested: Zionists claim statehood solved it; diasporists argue statehood created new existential dangers (militarization, occupation, antisemitism-by-association). The framework persists as a piton because its mandate (Jewish survival via pluralism) is neither fully live (Zionist hegemony claims to have superseded it) nor fully dead (diaspora Jews still exist and need survival strategies) — it is contested. The mandate has not been resolved; it has been suppressed. The theater_ratio captures the performative maintenance of institutions that no longer deliver the coordination they were built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the diasporist framework a distinct constraint on Jewish self-determination, or a reading of the ''Jewish self-determination'' kernel that competes with Zionist, religious, and settler-colonial readings?',
    'Decompose the kernel into separate constraint stories per the epsilon-invariance principle: each reading instantiates a different constraint with its own ε, beneficiaries, victims, and type. This story models the diasporist reading as one such constraint.',
    'If treated as a single constraint with measurement-dependent ε, the analysis conflates structurally distinct claims. Decomposition enables the engine to classify each reading independently and track their structural influence on each other via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this story models one reading of a contested kernel or a standalone constraint').

omega_variable(
    piton_vs_suppressed_rope,
    'Is the diasporist framework genuinely atrophied (piton — function lost to inertia) or actively suppressed but still functional (snare/tangled_rope — Zionist hegemony prevents its operation)?',
    'Compare diasporist institutional capacity in contexts with low Zionist influence (e.g., pre-1948 Poland, post-Soviet diaspora, certain Latin American communities) vs. high Zionist influence (US, UK, France). If the framework functions as a rope where Zionist hegemony is weak, it is suppressed, not atrophied.',
    'If suppressed rope, the constraint type should be tangled_rope (coordination function + asymmetric extraction by Zionist institutions) and the beneficiaries/victims structure shifts. If piton, the atrophied coordination function is intrinsic, not imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_suppressed_rope, empirical, 'Whether diasporist atrophy is intrinsic or imposed by Zionist hegemony').

omega_variable(
    diaspora_viability_long_term,
    'Is Jewish collective survival actually viable long-term in diaspora without territorial sovereignty, given rising ethnonationalism, antisemitism, and assimilation pressures?',
    'Longitudinal demographic and sociological study of Jewish continuity in diaspora vs. Israel over 50-100 years, controlling for host-state conditions. Historical test: 2000 years of diaspora survival vs. 75 years of statehood.',
    'If diaspora survival is empirically non-viable, the diasporist reading''s foundational axiom (diaspora_pluralism_secures_survival) is empirically_contingent and false — the constraint becomes a snare (false coordination story). If viable, the axiom holds and the piton classification reflects political suppression, not functional failure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_viability_long_term, empirical, 'Empirical viability of the diasporist survival strategy').

omega_variable(
    suppression_mechanism_zionist_hegemony,
    'Is the suppression of the diasporist framework primarily structural (institutional capture, resource control, definitional power) or internalized (Jews genuinely believe Zionism = Jewish survival, identity fusion with Israel)?',
    'Post-exit trajectory study: Jews who leave Zionist communal spaces — do they recover diasporist identity easily (suggesting structural suppression) or continue to feel ''not really Jewish'' without Israel (suggesting internalized suppression)? Survey data on Jewish identity attachment to Israel vs. diasporic culture.',
    'If internalized, the effective suppression is higher than structural measures suggest — the constraint''s victims carry the suppression with them. This would increase the piton''s effective extraction on payers and deepen the accessibility_collapse. If structural, liberation is possible through institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_zionist_hegemony, empirical, 'Structural vs. internalized suppression of the diasporist alternative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_diasporist_tr_t0, jewish_self_determination__diasporist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jsd_diasporist_tr_t20, jewish_self_determination__diasporist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jsd_diasporist_tr_t40, jewish_self_determination__diasporist_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(jsd_diasporist_tr_t60, jewish_self_determination__diasporist_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(jsd_diasporist_tr_t80, jewish_self_determination__diasporist_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement(jsd_diasporist_tr_t100, jewish_self_determination__diasporist_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(jsd_diasporist_be_t0, jewish_self_determination__diasporist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jsd_diasporist_be_t20, jewish_self_determination__diasporist_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(jsd_diasporist_be_t40, jewish_self_determination__diasporist_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(jsd_diasporist_be_t60, jewish_self_determination__diasporist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(jsd_diasporist_be_t80, jewish_self_determination__diasporist_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(jsd_diasporist_be_t100, jewish_self_determination__diasporist_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(jsd_diasporist_su_t0, jewish_self_determination__diasporist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(jsd_diasporist_su_t20, jewish_self_determination__diasporist_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(jsd_diasporist_su_t40, jewish_self_determination__diasporist_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(jsd_diasporist_su_t60, jewish_self_determination__diasporist_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(jsd_diasporist_su_t80, jewish_self_determination__diasporist_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(jsd_diasporist_su_t100, jewish_self_determination__diasporist_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint story is one of five readings of the 'jewish_self_determination' kernel. The kernel decomposes into: (1) diasporist_reading — this story, piton, moderate ε; (2) liberal_nationalist_reading — tangled_rope, moderate-high ε (state provides coordination but extracts via militarism); (3) indigenous_return_reading — rope or tangled_rope depending on Palestinian frame; (4) religious_covenant_reading — mountain (claimed) or snare (if extraction via settlement); (5) settler_colonial_reading — snare, high ε. The ε values differ structurally: diasporist ε reflects host-state vulnerability + Zionist suppression; liberal_nationalist ε reflects state maintenance costs; settler_colonial ε reflects dispossession. They are linked via affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, organized, 0.25).
constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, moderate, 0.85).
constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
