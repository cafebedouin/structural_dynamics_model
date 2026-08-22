% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Diaspora Pluralism and Minority Rights Reading of Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the diasporist reading of Jewish
 *   self-determination: the claim that Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism, minority-rights
 *   frameworks, and cultural autonomy within plural host societies—and that
 *   Zionism, by tying Jewish fate to a territorial state and militarized
 *   nationalism, represents a dangerous deviation from historically proven
 *   Jewish strategies. The constraint is operationalized as a piton: a
 *   once-robust framework of diaspora institutions, philosophical traditions,
 *   and political advocacy that has been substantially attenuated by Zionist
 *   institutional hegemony. The diasporist reading remains a live
 *   intellectual position (held by Jewish intellectuals, diaspora
 *   communities, and postcolonial theorists), but its institutional apparatus
 *   has been marginalized and its claim to speak for 'Jewish interest' has
 *   been suppressed by the organizational dominance of Zionist frameworks.
 *   The constraint is CLAIMED as piton because the diasporist
 *   alternative—once a primary mode of Jewish political thought and
 *   institutional organization—now persists mostly through theatrical
 *   maintenance and the resistance of intellectual and community remnants,
 *   while Zionist frameworks have captured the definition of mainstream
 *   Jewish identity and security.
 *
 * KEY AGENTS:
 *   - Diaspora Jewish communities: minority groups seeking cultural autonomy and security through pluralism
 *   - Zionist organizational apparatus: institutional networks defining Jewish interest through territorial sovereignty
 *   - Jews coerced into Zionist frameworks: individuals experiencing identity foreclosure and pressure to conform
 *   - Jews endangered by Israeli state actions: individuals whose safety is compromised by association with state violence
 *   - Diaspora philosophical and cultural institutions: Yiddish-speaking networks, minority-rights advocacy, ethical traditions
 *   - Palestinian communities: structurally excluded from Jewish self-determination conversation but directly affected
 *   - Host societies: nation-states whose capacity for minority protection shapes constraint viability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.62).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.71).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diaspora Pluralism and Minority Rights Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__diasporist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, 'ea41f290-3f4a-4173-b7f8-883c01ed51b4').
narrative_ontology:cs_kernel_codification('ea41f290-3f4a-4173-b7f8-883c01ed51b4', distributed).
narrative_ontology:cs_authority_grounding('ea41f290-3f4a-4173-b7f8-883c01ed51b4', distributed).
narrative_ontology:cs_reading_relation('ea41f290-3f4a-4173-b7f8-883c01ed51b4', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea41f290-3f4a-4173-b7f8-883c01ed51b4', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea41f290-3f4a-4173-b7f8-883c01ed51b4', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('ea41f290-3f4a-4173-b7f8-883c01ed51b4', jewish_self_determination__religious_covenant_reading, forecloses).
narrative_ontology:cs_axiom('ea41f290-3f4a-4173-b7f8-883c01ed51b4', foundational, diaspora_pluralism_superior_to_nationalism).
narrative_ontology:cs_axiom_status(diaspora_pluralism_superior_to_nationalism, holdable).
narrative_ontology:cs_axiom_grounding('ea41f290-3f4a-4173-b7f8-883c01ed51b4', diaspora_pluralism_superior_to_nationalism, deontological).
narrative_ontology:cs_axiom('ea41f290-3f4a-4173-b7f8-883c01ed51b4', foundational, minority_rights_adequate_for_jewish_security).
narrative_ontology:cs_axiom_status(minority_rights_adequate_for_jewish_security, holdable).
narrative_ontology:cs_axiom_grounding('ea41f290-3f4a-4173-b7f8-883c01ed51b4', minority_rights_adequate_for_jewish_security, empirically_contingent).
narrative_ontology:cs_axiom('ea41f290-3f4a-4173-b7f8-883c01ed51b4', secondary, territorial_nationalism_dangerous_to_jewish_ethics).
narrative_ontology:cs_axiom_status(territorial_nationalism_dangerous_to_jewish_ethics, holdable).
narrative_ontology:cs_axiom_grounding('ea41f290-3f4a-4173-b7f8-883c01ed51b4', territorial_nationalism_dangerous_to_jewish_ethics, deontological).
narrative_ontology:cs_reference_frame('ea41f290-3f4a-4173-b7f8-883c01ed51b4', diaspora_pluralism_as_historical_jewish_norm).
narrative_ontology:cs_drift_state('ea41f290-3f4a-4173-b7f8-883c01ed51b4', contemporary_zionist_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea41f290-3f4a-4173-b7f8-883c01ed51b4', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_state_actions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities across the diaspora maintain distinct cultural, linguistic, and religious identities within host societies. They benefit from frameworks that recognize Jews as minorities with rights to cultural autonomy, religious practice, and political participation as citizens of their respective nations. Their survival and flourishing depend on tolerance from host societies and on not being pressed into a singular identity defined by reference to a distant territorial state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, constrained, global).

% Yiddish-speaking cultural institutions, diaspora-rooted philosophical traditions, autonomous community governance structures, and minority-rights advocacy networks that sustained Jewish life across centuries. These institutions vindicate diaspora pluralism as a viable framework for Jewish continuity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_institutional_infrastructure, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__diasporist_reading, diaspora_institutional_infrastructure).

% Jews who prefer diaspora identity, linguistic tradition, or minority-rights frameworks but experience social and institutional pressure to adopt Zionist ideology as the primary definition of Jewish identity and interest. They bear the cost of identity foreclosure: abandoning or suppressing their own minority identity in favor of a nationalist frame, or facing marginalization as insufficiently committed to the reading's endpoint (territorial sovereignty).
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework, payer,
    powerless, biographical, identity_locked, global).

% Jews whose safety, reputation, and freedom of movement are compromised by association with Israeli state military actions, settlement policy, or occupation practices. They are made targets of anti-Jewish violence or discrimination because the reading's endpoint—the Zionist state—is identified as acting in their name. The constraint ties their Jewish identity to state violence they may not endorse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_state_actions, payer,
    powerless, biographical, identity_locked, global).

% Networks of organizations, foundations, political movements, and state institutions that define Jewish interest primarily through territorial sovereignty and Israeli security. They administer the constraint by controlling resource flows, defining Jewish institutional legitimacy, framing diaspora alternatives as unrealistic or self-hating, and marginalizing non-Zionist Jewish voices in mainstream Jewish organizational life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_organizational_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% The established state institutions, territorial claims, and political momentum that benefit from diaspora Jewish communities' resources, legitimacy, and political advocacy. The constraint ensures diaspora Jewish financial, diplomatic, and cultural support flows toward territorial state interests.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_political_settlements, beneficiary,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__diasporist_reading, zionist_political_settlements).

% Nation-states in which diaspora Jewish communities reside. They observe the constraint's operation as a claim about Jewish integration and identity. The constraint's assertion that Jews are best secured through minority rights frameworks makes a claim about the adequacy of secular citizenship and pluralism; host societies' capacity to protect minorities shapes whether the constraint remains viable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_societies_of_diaspora, observer,
    institutional, generational, analytical, national).

% Palestinian communities and regional powers whose interests are directly affected by Jewish territorial claims and Israeli state actions. The diasporist reading frames them as structurally excluded from the Jewish self-determination conversation—they are not seats at the Jewish identity table, yet the constraint's outcome (Zionism as dangerous deviation) directly implicates their land, political status, and security.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, palestinians_and_regional_observers, excluded,
    powerful, generational, trapped, regional).

% The interpretive and ethical traditions of diaspora Judaism—Talmudic argumentation, prophetic ethics emphasizing justice and hospitality, philosophy of minority coexistence—that vindicate the diasporist framework's claim that Jewish survival is compatible with, and strengthened by, pluralism and justice toward others.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jewish_philosophical_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__diasporist_reading, jewish_philosophical_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The diasporist reading solves the coordination problem of how Jewish communities maintain distinct identity and cultural continuity while participating as minorities in plural societies. It coordinates around the principle that Jews need not—and should not—depend on territorial sovereignty; minority rights frameworks, autonomous cultural institutions, and cosmopolitan philosophy provide sufficient and superior coordination for Jewish flourishing.
% TRANSFER_FUNCTION: Moves diaspora Jewish resources, political legitimacy, institutional autonomy, and identity definition away from territorial sovereignty frameworks and toward diaspora pluralism frameworks. Suppresses the organizational capacity of Zionist structures by redefining Jewish interest away from state-centered security and toward minority-rights advocacy and cultural autonomy.
% ABSENT_VOICES: Palestinians and other indigenous populations whose territorial and political claims are directly affected by the constraint's framing of Jewish self-determination. They are structurally excluded from the Jewish identity conversation yet affected by its outcome. Jewish voices advocating for Palestinian rights or questioning the binary choice between diaspora and territorial sovereignty are also partly excluded from mainstream diaspora institutional life—their concerns are marginalized as conflicting with Jewish security.
% DISAPPEARANCE_RATIONALE: If this constraint—the institutional and ideological apparatus asserting diaspora pluralism as the optimal Jewish framework—disappeared, the Zionist reading would face less organized intellectual and organizational opposition within Jewish communities, but diaspora institutions, Yiddish-language advocacy, and minority-rights frameworks would persist in decentralized form. The world would not 'rearrange'—rather, the balance of institutional power would shift further toward the territorial-sovereignty reading, making it harder (not impossible) for diaspora alternatives to maintain visibility. This contested verdict reflects the fact that the constraint's operation is already substantially attenuated by Zionist institutional hegemony.
% FOUNDING_PROBLEM: In the late 19th and early 20th centuries, diaspora Jewish communities faced rising antisemitism and pressure to assimilate while retaining distinct identity. The founding problem was: can Jewish communities survive and flourish as minorities with full cultural, religious, and political autonomy within plural host societies, or does survival require territorial sovereignty and a Jewish state? The diasporist reading asserts the former: diaspora pluralism, reinforced by minority-rights frameworks and cosmopolitan ethical traditions, is sufficient and preferable to territorial nationalism.
% FOUNDING_PROBLEM_CORROBORATION: Diaspora Jewish intellectuals, historians of Yiddish culture, and postcolonial theorists outside the Zionist establishment attest that diaspora survival has been historically demonstrated and that contemporary diaspora communities flourish under minority-rights frameworks. However, Zionist and Israeli scholars, and some diaspora Jewish organizational leadership, attest that historical persecution and 20th-century genocide demonstrate that minority status is fundamentally insecure and territorial sovereignty is necessary. The disagreement is not resolved by external corroboration—it is a live, unresolved dispute about historical causation and future security.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.62) because the constraint operates through identity suppression and institutional marginalization, not through direct coercion or resource expropriation. Suppression is higher (0.71) because maintaining Zionist ideological monopoly requires actively delegitimizing diaspora alternatives, marginalizing non-Zionist Jewish voices, and redefining Jewish identity in nationalist terms. Theater is high (0.58), which is the piton diagnostic: the constraint persists through institutional performance and symbolic maintenance (annual commemorations, organizational rituals, cultural claims) rather than through active coordination of material interests—diaspora institutions continue to exist and claim legitimacy, but their practical function has atrophied relative to Zionist institutional capacity. Accessibility of alternatives has partially collapsed (0.48) because the Zionist reading has achieved cultural hegemony in defining what 'Jewish interest' means, making it psychologically and institutionally difficult to articulate diaspora alternatives; however, collapse is incomplete because diaspora intellectual and community traditions persist. Resistance is substantial (0.64) because Jewish intellectuals, diaspora communities, and postcolonial theorists actively contest the Zionist monopoly on Jewish identity and refuse the binary choice between diaspora insecurity and territorial nationalism. The measurement series runs on a shared time grid (1880, 1920, 1948, 1967, 1990, 2026) showing the constraint's trajectory from a live institutional alternative in the early 20th century to an attenuated but persistent piton in the contemporary period.
 *
 * PERSPECTIVAL GAP:
 *   The Zionist organizational apparatus, as agenda-setter, holds the institutional power to define what counts as 'Jewish interest' and which voices are heard in mainstream Jewish institutional life. From this seat, the diasporist constraint is a threat to Jewish survival and must be suppressed through organizational marginalization, resource control, and ideological delegitimization. Diaspora Jewish communities and intellectuals occupy a different structural position: they are the beneficiaries of the constraint (it validates their identity and institutional traditions), but they lack the organizational apparatus to implement it on a large scale. They experience the constraint as a live intellectual and cultural position that is systematically suppressed by more powerful institutional actors. Jews who are identity-locked into the Zionist frame (whether through socialization, professional dependence on Zionist institutions, or internalization of the security narrative) experience the constraint very differently from Jews who maintain diaspora identity—the constraint operates for them as identity suppression, not identity validation.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities are structural beneficiaries: the constraint validates their identity, vindicates their institutional traditions, and asserts that Jewish survival is compatible with pluralism and minority status. Their directionality (d) should be near the beneficiary end (low d, yielding low or negative effective extraction). However, they also bear a cost through identity pressure and institutional marginalization—the constraint's call to diaspora pluralism is contested by more powerful Zionist frameworks, so the beneficiary position is constrained. Jews coerced into Zionist frameworks are targets (high d, high effective extraction): they experience identity foreclosure, pressure to conform, and the suppression of alternative frameworks. Jews endangered by Israeli state actions are targets (high d): their safety is compromised by association with a state they may not endorse. The Zionist organizational apparatus occupies the agenda-setter position: it sets the frame, controls resources, and marginalizes alternatives—it is structurally powerful and has arbitrage capacity (it can shift institutional resources toward different constituencies). Palestinian communities are excluded, not targets or beneficiaries—they have no voice in the Jewish self-determination framing, yet they are affected by its outcome. The directionality derivation produces the expected seat divergence: diaspora beneficiaries compute a low d; coerced and endangered Jews compute high d; excluded Palestinians compute no per-constraint directionality because they are outside the frame.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution requires analyzing the founding problem and its status. The founding problem (late 19th-century diaspora security and identity continuity) was live and pressing in the 1880–1920 period. By the 1948 establishment of Israel, the problem had shifted: the founding problem for the diasporist reading (can diaspora pluralism secure Jewish survival?) became contested—some read it as solved (diaspora communities did survive under minority-rights frameworks in the West), others read it as fatally undermined by the Holocaust and the need for Jewish state power. By 2026, the founding problem is clearly dead for the Zionist reading: Israeli statehood is established, and the question of whether to pursue territorial sovereignty is moot from their perspective (they have won it). But the founding problem is contested for the diasporist reading: they ask whether territorial sovereignty has actually enhanced Jewish security (evidence is mixed—Israel's establishment has produced persistent conflict, security threats to diaspora Jews through association, and existential debates about legitimacy), while minority-rights frameworks in liberal democracies have demonstrably sustained diaspora Jewish communities. The mandatrophy appears when we observe that the constraint (diasporist framework) claims to solve a founding problem (diaspora security) that its own reading declares contested, while the constraint's institutional apparatus has atrophied—it is maintained primarily through theatrical affirmation (cultural commemoration, philosophical argument) rather than through active institutional coordination. This is the piton diagnosis: the constraint's founding problem is unresolved, its function has degraded to performance, and the constraint persists by institutional inertia rather than through active coordination of parties who benefit from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diaspora_security_empirical_claim,
    'Has diaspora minority status historically provided adequate security for Jewish communities, or has the Holocaust and contemporary antisemitism demonstrated that territorial sovereignty is necessary for Jewish security?',
    'Historical analysis of Jewish security outcomes across diaspora and territorial contexts; comparative study of threat trajectories in liberal democracies vs. nation-states with Jewish majorities; assessment of whether Israeli statehood has reduced or increased Jewish security globally.',
    'If diaspora frameworks have provided demonstrably superior security outcomes, the diasporist reading''s founding problem remains live and the constraint''s claim is vindicated. If territorial sovereignty has provided superior security, the founding problem is dead and mandatrophy is confirmed—the constraint persists as theatrical maintenance despite its founding rationale being defeated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_security_empirical_claim, empirical, 'Whether historical evidence supports diaspora or territorial models for Jewish security.').

omega_variable(
    identity_lock_mechanism_diaspora,
    'Is the suppression of diaspora frameworks among Jews structural (institutional control, resource dependency, ideological hegemony) or internalized (Jews have come to genuinely believe Zionist framing of security)?',
    'Post-Zionist generational analysis: if younger Jews who are less socialized into Zionist institutional frameworks show higher comfort with diaspora alternatives and pluralism, suppression is primarily structural; if the discomfort persists, internalization has occurred.',
    'If suppression is primarily structural, weakening Zionist institutional control could revive the diasporist constraint; if it is internalized, the constraint would require identity-frame shifting to become viable again. The distinction affects the mechanism of potential institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_diaspora, empirical, 'Whether identity suppression is structural or internalized in diaspora Jewish communities.').

omega_variable(
    reading_coherence_pluralism_claim,
    'Is the diasporist reading internally coherent in claiming both that diaspora pluralism is viable AND that Zionism is a dangerous deviation—or does the reading require that diaspora communities maintain ideological purity incompatible with the pluralism it advocates?',
    'Philosophical analysis of whether diaspora pluralism can accommodate Zionist Jews within it, or whether the framework logically requires the suppression or exit of those who advocate territorial nationalism.',
    'If the reading is internally incoherent (requiring suppression of Zionism to maintain pluralism), it defeats its own normative claim and becomes a snare disguised as a rope. If coherent, the reading can accommodate internal disagreement and maintain its piton status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coherence_pluralism_claim, conceptual, 'Logical coherence of diaspora pluralism framework and its tolerance for Zionist dissent.').

omega_variable(
    reading_foundation_committer,
    'This constraint is one reading of jewish_self_determination kernel. What distinguishes the diasporist reading from its sibling readings? (Route committer structure per Rule 2.)',
    'Comparison of foundational axioms across readings: diasporist privileges security through minority-rights integration and ethical pluralism; liberal nationalist privileges equal claim to national self-determination; indigenous return privileges land connection and decolonization; settler colonial privileges Palestinian indigenous claims; religious covenant privileges theological obligation. Each reading has different answers to ''What makes a Jewish claim legitimate?'' and ''What secures Jewish survival?''',
    'Understanding the axioms'' grounding types reveals which readings are empirically contestable (empirically_contingent axioms) vs. normatively foundational (deontological or theological). Diasporist reading rests partly on empirical claims about historical security (empirically_contingent: diaspora works) and partly on normative claims about pluralism''s superiority (deontological: justice toward minorities).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foundation_committer, conceptual, 'Axioms distinguishing diasporist reading from sibling readings and their grounding types.').

omega_variable(
    excluded_palestinians_voice,
    'Can a reading of Jewish self-determination be adequate if it does not center Palestinian voice and indigenous claims, or is the diasporist reading incoherent precisely because it calls for Jewish security through pluralism while leaving Palestinians excluded from the definitional frame?',
    'Critical analysis of whether diasporist Jewish intellectuals engage Palestinian political claims as equal parties in the self-determination conversation, or whether the reading remains Jewish-centric even in its pluralism advocacy.',
    'If the diasporist reading fails to center Palestinian voice, it is not truly pluralist—it is a piton that performs pluralism while maintaining Jewish institutional autonomy. If it can genuinely incorporate Palestinian claims, it becomes a more robust rope-type coordination (though this would require fundamental revision of what ''Jewish self-determination'' means).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_palestinians_voice, preference, 'Whether diasporist reading''s pluralism extends to Palestinian self-determination or remains Jewish-centric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1880, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_self_determination__diasporist_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(jewi_tr_t1920, jewish_self_determination__diasporist_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.48).
narrative_ontology:measurement(jewi_tr_t1990, jewish_self_determination__diasporist_reading, theater_ratio, 1990, 0.54).
narrative_ontology:measurement(jewi_tr_t2026, jewish_self_determination__diasporist_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_self_determination__diasporist_reading, base_extractiveness, 1880, 0.25).
narrative_ontology:measurement(jewi_be_t1920, jewish_self_determination__diasporist_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.52).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement(jewi_be_t1990, jewish_self_determination__diasporist_reading, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement(jewi_be_t2026, jewish_self_determination__diasporist_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_self_determination__diasporist_reading, suppression_requirement, 1880, 0.18).
narrative_ontology:measurement(jewi_su_t1920, jewish_self_determination__diasporist_reading, suppression_requirement, 1920, 0.31).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.44).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(jewi_su_t1990, jewish_self_determination__diasporist_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(jewi_su_t2026, jewish_self_determination__diasporist_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel admits five structurally distinct constraint readings, each with different epsilon values, beneficiary/victim structures, and type classifications. The diasporist_reading instantiates one of these: it claims piton status (attenuated diaspora institutions maintained against Zionist hegemony). Other readings (liberal_nationalist, indigenous_return, settler_colonial, religious_covenant) each present different ε values and different structural relationships. This is not observable-dependence within one constraint—it is genuine constraint decomposition: different readings of the kernel produce different constraints because they pivot on different core premises about what makes a Jewish claim legitimate and what secures Jewish survival. All five readings are linked via network.affects_constraints because they share a kernel and compete for institutional and political dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
