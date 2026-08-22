% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination as Equivalent National Claim (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The liberal-nationalist reading frames Jewish self-determination as an
 *   instance of the universal principle that all peoples have equal right to
 *   national sovereignty and self-governance. It asserts that Jewish people
 *   constitute a people/nation in the same sense as other nations, and
 *   therefore have the same claim to territorial self-determination that is
 *   recognized for other nations. This reading emerged in late 19th-century
 *   European Zionism but claims universalist, secular foundations rooted in
 *   liberal political philosophy and Enlightenment nationalism. The
 *   constraint under examination is the principle itself: that Jewish people
 *   have equal self-determination claims as other peoples. Under this
 *   reading, this principle solves the coordination problem of how a
 *   dispersed, persecuted people can secure survival and dignity in a world
 *   organized around territorial nation-states. The reading does NOT claim
 *   Jewish superiority or unique historical rights; it claims equality of
 *   rights within a universal framework.
 *
 * KEY AGENTS:
 *   - Jewish diaspora communities across Europe and Middle East — seek refuge and sovereignty under the universal self-determination principle
 *   - Liberal political theorists and philosophers — articulate and defend the universal principle of self-determination
 *   - International liberal-democratic order — benefits from universalizing the principle (applies equally to all peoples)
 *   - Palestinian Arabs and inhabitants — positioned as having countervailing claims under the same universal principle; experience extractive costs if partition implementation prioritizes Jewish self-determination
 *   - Diasporist Jewish communities (excluded) — argue the reading misrepresents Jewish interests and falsely narrows Jewish options
 *   - Religious covenant readers (excluded) — argue Jewish claim derives from divine covenant, not secular universalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.48).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.52).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish Self-Determination as Equivalent National Claim (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'b0091b54-8af7-49c3-80dd-30aef97e2aaf').
narrative_ontology:cs_kernel_codification('b0091b54-8af7-49c3-80dd-30aef97e2aaf', formalized).
narrative_ontology:cs_authority_grounding('b0091b54-8af7-49c3-80dd-30aef97e2aaf', expertise).
narrative_ontology:cs_interpretation_layer_present('b0091b54-8af7-49c3-80dd-30aef97e2aaf').
narrative_ontology:cs_reading_relation('b0091b54-8af7-49c3-80dd-30aef97e2aaf', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0091b54-8af7-49c3-80dd-30aef97e2aaf', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('b0091b54-8af7-49c3-80dd-30aef97e2aaf', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('b0091b54-8af7-49c3-80dd-30aef97e2aaf', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('b0091b54-8af7-49c3-80dd-30aef97e2aaf', foundational, universal_self_determination_principle).
narrative_ontology:cs_axiom_status(universal_self_determination_principle, holdable).
narrative_ontology:cs_axiom_grounding('b0091b54-8af7-49c3-80dd-30aef97e2aaf', universal_self_determination_principle, deontological).
narrative_ontology:cs_axiom('b0091b54-8af7-49c3-80dd-30aef97e2aaf', foundational, secular_universalism_supersedes_particularism).
narrative_ontology:cs_axiom_status(secular_universalism_supersedes_particularism, holdable).
narrative_ontology:cs_axiom_grounding('b0091b54-8af7-49c3-80dd-30aef97e2aaf', secular_universalism_supersedes_particularism, deontological).
narrative_ontology:cs_axiom('b0091b54-8af7-49c3-80dd-30aef97e2aaf', secondary, ethnic_national_organization_legitimate).
narrative_ontology:cs_axiom_status(ethnic_national_organization_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('b0091b54-8af7-49c3-80dd-30aef97e2aaf', ethnic_national_organization_legitimate, conventional).
narrative_ontology:cs_reference_frame('b0091b54-8af7-49c3-80dd-30aef97e2aaf', liberal_nationalist_universalism_framework).
narrative_ontology:cs_drift_state('b0091b54-8af7-49c3-80dd-30aef97e2aaf', contemporary_postcolonial_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b0091b54-8af7-49c3-80dd-30aef97e2aaf', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, international_liberal_democratic_order).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_arabs_and_inhabitants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed Jewish communities across Europe, North Africa, and Middle East seeking refuge from persecution and a territorial base for collective self-determination. Under this reading, they claim the same universal right to national sovereignty that other peoples exercise. The constraint provides normative grounding for sovereignty claims and political mobilization toward a Jewish state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty, beneficiary,
    organized, generational, constrained, global).

% The liberal-nationalist framework vindicates a universal principle of self-determination that applies equally to all peoples, including Jews. This universalizes the legitimacy of nation-states and creates a consistent framework for resolving competing territorial claims through consent and partition rather than religious or historical primacy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_liberal_democratic_order, beneficiary,
    institutional, generational, analytical, universal).

% Under this reading's framework, Palestinians are positioned as having countervailing self-determination claims that must be negotiated against Jewish claims. If implementation proceeds as partition-and-separation, they bear the cost of territorial concession and political division. The constraint does not name Palestinians as beneficiaries even though they too invoke self-determination; the reading's structural position creates asymmetry in how claims are weighted.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_arabs_and_inhabitants, payer,
    moderate, generational, trapped, regional).

% Articulate and defend the principle that all peoples have equal claim to self-determination regardless of religion, ethnicity, or historical connection. They set the conceptual framework within which Jewish self-determination claims are voiced and evaluated. They maintain the reading's intellectual authority and respond to critiques.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_political_theorists_and_philosophers, agenda_setter,
    institutional, generational, analytical, global).

% Jewish communities and intellectuals who reject territorial sovereignty as either unnecessary or dangerous, arguing instead for diaspora pluralism and embeddedness in liberal democracies. They would argue the constraint falsely narrows Jewish options to either territorial state or minority status, and creates obligations to participate in nationalist projects they reject.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, diasporist_jewish_communities, excluded,
    organized, biographical, constrained, global).

% Other peoples asserting self-determination claims (Palestinians, Kurds, Rohingya, etc.). Under the liberal-nationalist reading, they have the same rights and standing. The constraint creates both validation and pressure: it validates their claims universally but also structures them as necessarily competitive and requiring negotiated partition.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, competing_national_movements, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a dispersed people can exercise collective self-determination in a world of territorial nation-states. The constraint provides a normative principle (all peoples have equal claim to self-determination) that removes the burden of proving special historical, religious, or racial exceptionalism and instead rests the claim on universal rights applicable to any human collective identifying as a people.
% TRANSFER_FUNCTION: Moves legitimacy and institutional recognition from ethnic or religious primacy (Jewish people have divine claim or unbroken historical connection) to universal secular principles (any people meeting the criteria of collective identity and will to sovereignty has an equal claim). This shifts the frame from particularism to universalism.
% ABSENT_VOICES: Diasporist Jews who reject the sovereignty frame and believe it misrepresents Jewish interests and aspirations; Palestinians and other indigenous populations whose claims would collide with Jewish self-determination and who are not represented by the reading's own internal logic (they appear as negotiating parties, not as beneficiaries of the universal principle); secular universalists who argue national self-determination is parochial and should yield to individual rights and cosmopolitan governance.
% DISAPPEARANCE_RATIONALE: If this constraint (the principle that Jewish people have equal claim to self-determination as other peoples) disappeared, the world would not rearrange completely, but it would shift substantially. Without it, Jewish sovereignty claims would need to rest entirely on religious, historical, or ethnic exceptionalism — which is precisely what the liberal-nationalist reading attempts to escape. The principle enables a particular framing of Israeli statehood as the exercise of a universal right, rather than a special exception. Some argue the world would stabilize better if the principle were abandoned and replaced by transnational or post-national frameworks; others argue it is already defunct and Israeli statehood rests on facts-on-the-ground rather than legitimacy.
% FOUNDING_PROBLEM: Jewish diaspora communities across Europe and the Middle East faced recurrent persecution, exclusion, and insecurity in the nineteenth and twentieth centuries, with no clear path to safety within liberal-democratic frameworks that promised equal citizenship but delivered discrimination and violence (pogroms, legal disabilities, genocide). The founding problem is: how can a people secure survival and dignity when liberal democracies cannot or will not protect them?
% FOUNDING_PROBLEM_CORROBORATION: Jewish historians and political theorists (Herzl, Bauer, Mendelsohn, Scholem) attest the persecution is historical fact and that liberal assimilation failed to deliver security. Historians outside the Jewish national movement (Hobsbawm, Anderson, Gellner) attest that persecution of minorities in European contexts was structural and that minority rights frameworks of the nineteenth century were inadequate. However, the claim that territorial sovereignty is the ONLY or BEST solution is contested: diasporist theorists argue alternative frameworks (internationalism, minority-rights law, cosmopolitan governance) would have solved the problem more justly; postcolonial scholars argue the territorial solution was borrowed from European nationalism and imposed on a different context; Palestinian historians argue the founding problem was solved at their expense rather than solved at all.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at end of interval) because the principle itself is non-extractive at the universal level — it vindicates equal rights. However, extractiveness is not zero because: (1) the reading's implementation as territorial partition necessarily privileges Jewish demographic claims over Palestinian claims in contested territory, creating asymmetric costs; (2) the reading assumes a particular definition of 'people' (ethnic-national) that excludes other organizational frameworks (diaspora, transnational, post-national); (3) suppression is needed to defend the territorial partition against Palestinian counterclaims. Suppression is significant (0.52) because the reading's political realization requires either suppressing or subordinating Palestinian self-determination claims or negotiating their equal satisfaction through partition — a coordination problem that has not yielded a stable solution. Theater ratio is low (0.22) because the reading's intellectual and political function is primarily substantive — it provides genuine normative justification for statehood — though some observance is performative (ritualized appeal to universal principles while particular ethnic-national interests drive policy). The measurement series tracks extractiveness rising sharply through 1947–1967 (as the constraint moves from philosophical principle to state apparatus and occupation) and then stabilizing at a contested equilibrium. This is consistent with the reading generating both authentic coordination (diaspora Jews securing a refuge) and authentic extraction (Palestinian displacement and subordination).
 *
 * PERSPECTIVAL GAP:
 *   The liberal-nationalist reading produces asymmetric seats with different computed types: (1) From the Jewish diaspora beneficiary seat, the constraint is a genuine rope — it solves the real coordination problem of how a persecuted people secures survival. (2) From the international liberal-democratic order's analytical seat, the constraint is a rope — it vindicates a universal principle and provides consistent grounds for resolving territorial disputes. (3) From the Palestinian payer seat, the constraint computes as tangled_rope or snare, depending on whether implementation is negotiated partition (tangled_rope: genuine problem-solving under asymmetric cost) or unilateral appropriation (snare: extraction with suppression). The engine should compute these divergent types from the structural data: beneficiary exit is 'constrained' (reliance on the state for safety), agenda-setter power is 'institutional' (liberal-democratic order's legitimacy authority), payer exit is 'trapped' (no exit from the territorial claim), and suppression is high (needed to maintain partition). The reading's design is non-extractive at the universal principle level but extractive at the territorial implementation level — this explains the moderate extractiveness and the high suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora beneficiary: low directionality (d ≈ 0.2–0.3), strong beneficiary position. The constraint provides normative grounding for their sovereignty claim and exempts them from having to prove exceptionalism. Their exit options are constrained (reliance on the state for security), which would normally increase d, but their role as beneficiaries (collecting the legitimacy and political recognition) dominates. Liberal-democratic order: d ≈ 0.0 (analytical, observer-class directionality), benefits from universalizing self-determination principles. Palestinian Arabs: high directionality (d ≈ 0.8–0.9), strong target position. The reading's implementation as partition requires them to concede territory and accept subordinate political status or continued conflict. Their exit is trapped (territorial claims cannot be exited without abandoning collective identity). Diasporist Jewish communities: d ≈ 0.6 (moderate target position), excluded from the reading's beneficiary coalition and forced to either accept national sovereignty as binding or maintain a dissenting position. Competing national movements (Kurds, Rohingya, etc.): d ≈ 0.5 (symmetric), benefit from the universalizing principle but face the same coordination problem of whether partition or coexistence is feasible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (persecution of diaspora Jews, insecurity under minority status) was genuine at the reading's origin (late 19th century) and remains contested today. The claim that territorial sovereignty is the solution depends on whether liberal-democratic frameworks could have been reformed to offer genuine security and dignity to minorities. If they could have been reformed, the constraint may represent a false choice — presenting territorial nationalism as necessary when alternative frameworks existed. However, the empirical record (Holocaust, pogroms, legal disabilities, and persistent antisemitism even in liberal democracies) supports that liberal frameworks failed to protect Jewish safety in the historical conditions the reading responds to. The problem persists in present form: Jewish communities continue to face existential threats and discrimination in some contexts, though this is now geographically variable rather than universal. The reading avoids pure mandatrophy (orphaned constraint serving no function) because it continues to provide legitimacy for Israeli statehood and remains invoked in diaspora Jewish communities. However, mandatrophy is beginning to appear in the settler-colonial critique: if the founding problem (diaspora insecurity) is no longer the primary driver of Israeli policy, and if Israeli state interests have shifted to territorial expansion and resource control, then the reading's intellectual function has decoupled from its political practice. The reading should be monitored for advancement into piton status — performative appeal to self-determination principles while actual policy violates them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_principle_vs_particularism,
    'Is self-determination genuinely universal in application, or does the liberal-nationalist framework privilege certain peoples'' claims (those organized as nations) over others (indigenous peoples, diasporics, stateless groups)?',
    'Systematic comparison of how the principle is applied to different peoples: do national claims from Europe, Asia, Africa, and the Middle East receive equal weight in international forums? Are indigenous territorial claims treated with the same urgency as settler-national claims?',
    'If the principle is selectively applied, the constraint''s claim to universality fails and it functions instead as a particular justification for specific state projects. If genuinely universal, it validates all national self-determination claims equally and the Palestinian-Israeli conflict is a case of competing valid claims requiring negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_principle_vs_particularism, empirical, 'Whether liberal-nationalist self-determination is genuinely universal or selectively applied to certain peoples.').

omega_variable(
    partition_vs_coexistence,
    'Does the liberal-nationalist framework require territorial separation (partition into ethnically distinct nation-states) or can it accommodate shared sovereignty, power-sharing, or non-territorial national autonomy?',
    'Theoretical analysis of whether the principle logically necessitates partition or whether alternatives are consistent with it; empirical cases where national groups coexist within shared political structures (Switzerland, Belgium, multinational federations).',
    'If partition is structurally necessary to the reading, then implementation requires displacement of one group by another, making the extractiveness higher than the base measurement suggests. If coexistence is possible, the reading opens pathways that do not require a zero-sum territorial division.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_coexistence, conceptual, 'Whether self-determination logically requires territorial partition or permits coexistence frameworks.').

omega_variable(
    secular_foundation_stability,
    'How stable is the secular universalist foundation of this reading when confronted with historical religious and ethnic claims to the same territory?',
    'Analysis of how the reading handles competing non-secular foundational claims (religious covenant, indigenous connection, historical possession). Observation of whether, in practice, the secular principle dominates or whether religious/historical claims reassert themselves in political practice.',
    'If secular universalism is brittle and religious/ethnic particularism reasserts, the reading''s actual legitimacy is not universal self-determination but particular ethno-national claims. This would shift classification toward false-summit dynamics (a constructed constraint disguised as universal principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_foundation_stability, conceptual, 'Whether secular liberal-nationalist foundations can sustain themselves against particularist historical and religious claims.').

omega_variable(
    reading_kernel_relationship,
    'Which sibling readings of the jewish_self_determination kernel share the liberal-nationalist reading''s secular universalist premise, and which reject it fundamentally?',
    'Classification of each sibling reading by its foundational epistemology: does it rest on universal secular principles (liberal_nationalist, diasporist as counter-universal), or on particularist claims (indigenous_return, religious_covenant, settler_colonial as anti-universal)?',
    'This classification determines which readings can coexist within a single framework and which fundamentally foreclose each other. High-confidence resolution is necessary for the reading_relations array in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Structural relationship between liberal-nationalist universalism and sibling readings'' epistemologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(jewi_tr_t1920, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(jewi_tr_t1947, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1947, 0.18).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(jewi_tr_t1995, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1880, 0.22).
narrative_ontology:measurement(jewi_be_t1920, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(jewi_be_t1947, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1947, 0.41).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement(jewi_be_t1995, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1880, 0.15).
narrative_ontology:measurement(jewi_su_t1920, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1920, 0.28).
narrative_ontology:measurement(jewi_su_t1947, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1947, 0.38).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(jewi_su_t1995, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__liberal_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, palestinian_self_determination__nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, universal_self_determination_principle).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, ethnic_nation_state_legitimacy).

% DUAL FORMULATION NOTE:
% The liberal-nationalist reading is one of five sibling readings of the jewish_self_determination kernel. It differs from siblings in its secular, universalist epistemology (liberal_nationalist claims universal applicability; indigenous_return and religious_covenant claim particularity). The reading affects constraint_palestinian_self_determination__nationalist_reading as an upstream competing claim — both invoke the same universal principle but for different peoples in the same territory. The reading also affects universal_self_determination_principle and ethnic_nation_state_legitimacy as both upstream and downstream influences: liberal-nationalism vindicates the universal principle but also crystallizes it into a particular state project, creating tension between principle and practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
