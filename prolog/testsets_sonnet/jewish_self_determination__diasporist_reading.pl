% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Diasporist Reading: Minority-Rights Pluralism as the Authentic Vehicle of Jewish Survival
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates the diasporist reading of the
 *   jewish_self_determination kernel: the claim that Jewish collective
 *   flourishing is best secured through pluralist minority-rights integration
 *   into host states rather than through territorial sovereignty, and that
 *   Zionism represents a strategic and moral deviation that ties Jewish
 *   safety to the fortunes and conduct of a militarized state. The reading is
 *   authored here as a piton — a formerly live, institutionally serious
 *   alternative (Bundism, Dubnow's autonomism, territorialism) whose
 *   coordination function (securing Jewish safety and cultural continuity
 *   without statehood) has atrophied under historical pressure (the
 *   Holocaust's destruction of its East European institutional base, the 1948
 *   establishment of Israel, and subsequent consolidation of diaspora
 *   communal funding and definitional authority around Zionist-aligned
 *   establishment bodies) while the normative claim persists, increasingly
 *   maintained by advocacy and theatrical assertion rather than by a
 *   functioning, well-resourced institutional alternative. This is NOT the
 *   same constraint as the liberal_nationalist_reading (which holds Jewish
 *   peoplehood grounds an equal claim to self-determination through
 *   statehood), nor the indigenous_return_reading, settler_colonial_reading,
 *   or religious_covenant_reading — each of those instantiates a structurally
 *   distinct claim with its own beneficiary/victim structure and its own
 *   epsilon, and is authored as a separate constraint story linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - diaspora_communal_institutions: agenda_setter (organized/constrained) — administers pluralist communal infrastructure with shrinking resources
 *   - diaspora_jews_maintaining_distinct_identity: beneficiary (moderate/constrained) — validated by the reading but institutionally underserved
 *   - jews_coerced_into_zionist_framework: payer (powerless/trapped) — communal participation conditioned on Zionist affiliation
 *   - diaspora_jews_endangered_by_israeli_state_association: payer (powerless/trapped) — bear externally imputed association without diasporist alternative reaching them
 *   - anti_zionist_jewish_dissenters: payer/excluded (powerless/constrained) — sanctioned for articulating the reading itself
 *   - zionist_communal_establishment: excluded from this reading's internal account — the force whose dominance the reading critiques
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.48).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.55).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading: Minority-Rights Pluralism as the Authentic Vehicle of Jewish Survival").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '65474b00-8f12-47be-9bb1-fc7b7a07a79e').
narrative_ontology:cs_kernel_codification('65474b00-8f12-47be-9bb1-fc7b7a07a79e', distributed).
narrative_ontology:cs_authority_grounding('65474b00-8f12-47be-9bb1-fc7b7a07a79e', distributed).
narrative_ontology:cs_reading_relation('65474b00-8f12-47be-9bb1-fc7b7a07a79e', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('65474b00-8f12-47be-9bb1-fc7b7a07a79e', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('65474b00-8f12-47be-9bb1-fc7b7a07a79e', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('65474b00-8f12-47be-9bb1-fc7b7a07a79e', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('65474b00-8f12-47be-9bb1-fc7b7a07a79e', foundational, territorial_sovereignty_unnecessary_for_survival).
narrative_ontology:cs_axiom_status(territorial_sovereignty_unnecessary_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('65474b00-8f12-47be-9bb1-fc7b7a07a79e', territorial_sovereignty_unnecessary_for_survival, empirically_contingent).
narrative_ontology:cs_axiom('65474b00-8f12-47be-9bb1-fc7b7a07a79e', secondary, state_militarization_endangers_diaspora_safety).
narrative_ontology:cs_axiom_status(state_militarization_endangers_diaspora_safety, holdable).
narrative_ontology:cs_axiom_grounding('65474b00-8f12-47be-9bb1-fc7b7a07a79e', state_militarization_endangers_diaspora_safety, instrumental).
narrative_ontology:cs_reference_frame('65474b00-8f12-47be-9bb1-fc7b7a07a79e', autonomist_diaspora_nationalism).
narrative_ontology:cs_drift_state('65474b00-8f12-47be-9bb1-fc7b7a07a79e', post_1948_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('65474b00-8f12-47be-9bb1-fc7b7a07a79e', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_communal_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jews_maintaining_distinct_identity).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_jews_endangered_by_israeli_state_association).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, anti_zionist_jewish_dissenters).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, minority_rights_regime_viability).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, galut_nationalism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bundist-descended and liberal diaspora organizations (cultural federations, Yiddishist and Ladino cultural bodies, minority-rights advocacy groups) administer communal life on the premise that pluralist coexistence within host states, not a state of their own, is the durable vehicle for Jewish continuity. They could, in principle, reorient toward Zionist federation structures but bear the cost of doing so themselves — funding streams, communal prestige, and youth engagement have shifted toward Israel-centered institutions, and reasserting the diasporist model requires resources these bodies increasingly lack.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_communal_institutions, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, diaspora_communal_institutions, beneficiary).

% Jews who build their communal and cultural life around host-country citizenship and minority-rights protections rather than aliyah or state-centered identification. They benefit from a framework that validates diaspora existence as complete rather than provisional, but that framework's institutional infrastructure (Yiddish schools, autonomist federations, non-Zionist landsmanshaftn) has thinned, leaving them dependent on host-state tolerance that is not guaranteed.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jews_maintaining_distinct_identity, beneficiary,
    moderate, biographical, constrained, national).

% Individuals and communities for whom communal, religious, or social participation has been made conditional on affirming Israel-centered Jewish identity — synagogue membership, Jewish day school admission, or family standing tied to Zionist affiliation. They experience the collapse of the diasporist alternative as a real constraint on their options, not an abstract debate; dissent risks communal ostracism.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework, payer,
    powerless, biographical, trapped, national).

% Diaspora Jews who face antisemitic backlash, surveillance, or violence tied to their perceived (often externally imputed) association with Israeli state conduct, regardless of their own political views. They cannot opt out of the association because it is assigned to them by hostile external actors, and the diasporist institutions that might have offered an alternative public identity have limited reach or funding to contest the imputation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jews_endangered_by_israeli_state_association, payer,
    powerless, immediate, trapped, global).

% Jews who actively organize around the diasporist critique of Zionism and face communal sanction — deplatforming from mainstream Jewish institutions, accusations of self-hatred, loss of professional or communal standing. They are the strongest advocates of this reading but are also its clearest victims, since asserting it carries direct social and institutional cost within a communal landscape now organized substantially around Zionist consensus.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, anti_zionist_jewish_dissenters, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, anti_zionist_jewish_dissenters, excluded).

% States within which diaspora communities are embedded, whose minority-rights frameworks and degree of tolerance determine whether diasporism is viable as a real alternative or merely a normative claim. Their policies (equal citizenship guarantees, protection against discrimination, or conversely rising ethno-nationalism and antisemitism) directly determine the diasporist reading's real-world footing without host states themselves being parties to the internal Jewish debate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_state_governments, observer,
    institutional, generational, analytical, national).

% Not named as a stakeholder inside this reading's own account, but structurally present as the force whose institutional dominance the diasporist reading identifies as having captured 'Jewish interest' representation, communal funding, and definitional authority over antisemitism and Jewish identity. Their perspective on this same kernel appears in the sibling liberal_nationalist_reading and religious_covenant_reading constraints, not here.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_communal_establishment, excluded,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Diasporist pluralism genuinely solves a coordination problem: it provides a framework for Jewish communities to secure legal equality, cultural autonomy, and political voice within host states without requiring territorial concentration or military capacity — a workable model precedented by autonomist and Bundist movements and by centuries of diaspora communal self-governance.
% TRANSFER_FUNCTION: The reading moves communal legitimacy, institutional funding, and definitional authority over 'authentic Jewish interest' away from Zionist-aligned federations and toward pluralist, minority-rights-oriented diaspora institutions — and asks individual Jews to bear the reputational and physical risk of publicly maintaining a non-state-centered Jewish identity in a landscape where that identity is increasingly treated as marginal or suspect by both external antisemites and internal communal gatekeepers.
% ABSENT_VOICES: Zionist communal institutions and their diaspora constituents are structurally absent from this reading's own internal account — they appear as the atrophying force being critiqued, not as an interlocutor with a stake being weighed. Palestinian voices are also absent: this reading contests Jewish political strategy on its own terms and does not by itself adjudicate the settler-colonial or indigenous-return claims about the land itself.
% DISAPPEARANCE_RATIONALE: If the diasporist reading vanished entirely from Jewish political discourse, its own adherents argue that a historically legitimate and once-dominant model of Jewish survival (autonomism, Bundism, liberal minority-rights integration) would be erased, narrowing legitimate Jewish political identity to a single state-centered template and silencing communities for whom diaspora life is not provisional. Critics of the reading argue the world would barely change, since the diasporist institutional base is already so attenuated that its disappearance would mostly formalize a fait accompli produced by twentieth-century catastrophe and post-1948 institutional consolidation, not by any live suppression.
% FOUNDING_PROBLEM: In the late 19th and early 20th centuries, diasporist and autonomist Jewish movements (Bundism, Dubnow's autonomism, territorialist alternatives) were built to answer the same problem Zionism answered — antisemitic violence and legal precarity in Europe — but through minority-rights guarantees and cultural autonomy within existing states rather than through a Jewish state, on the argument that territorial nationalism would be more dangerous and less achievable than legal and cultural emancipation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Bund and of interwar Jewish autonomism (writing from academic institutions with no stake in either communal establishment) attest that the founding problem was real and that the diasporist answer was a serious contemporary contender to Zionism, not a fringe position — but they also document that the Holocaust's destruction of the Bund's East European base and the 1948 establishment of Israel radically altered the comparative plausibility of the two answers in ways diasporist advocates dispute rather than fully concede. No corroboration exists from a source that is simultaneously outside both the Zionist establishment and the diasporist advocacy tradition on whether the underlying problem remains live today; this absence is itself noted rather than papered over.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.48, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as moderate (0.48) because the diasporist reading does not itself extract in the manner of an enforced hierarchy — its harms flow from atrophy (the alternative it names has weakened) rather than from active predation, but the reading's persistence does impose real costs on dissenters and on Jews whose identity does not map cleanly onto either camp. Suppression (0.55) is authored as substantial but not severe: the reading is not criminalized, but communal gatekeeping, funding structures, and definitional battles over antisemitism impose real costs on its public advocates. Theater ratio rises sharply across the interval (0.10 to 0.62) because the coordination function the reading names — actual functioning autonomist/pluralist institutions capable of securing Jewish safety without statehood — has substantially atrophied since 1948, while assertion of the normative claim (that this path remains viable and superior) has if anything intensified in post-2000 anti-Zionist and non-Zionist Jewish discourse, a classic piton signature: performance outpacing function. Accessibility collapse (0.58) reflects that the diasporist institutional alternative, while not impossible, has become genuinely harder to access as communal infrastructure consolidated elsewhere. Resistance (0.47) reflects real but constrained pushback — advocates persist and organize, but face communal and reputational sanction that dampens the resistance's visibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communal institutions and diaspora Jews who maintain non-Zionist identity are the structural beneficiaries: the reading validates their existing mode of life and gives it a coherent normative account. Jews coerced into Zionist-affiliated communal participation, Jews endangered by imputed association with Israeli state conduct, and anti-Zionist Jewish dissenters who face communal sanction are the structural targets — the first two bear costs from the reading's underlying institutional weakness (there is no robust alternative infrastructure to fall back on), and the third bears direct costs from asserting the reading itself in a hostile communal environment. This is a piton-typical directionality profile: the 'beneficiaries' benefit from a validated identity, not from extraction of resources from others, while the 'victims' pay through exposure and constraint rather than through captured rents flowing to a concentrated beneficiary — consistent with the piton signature that no party profits enough from this arrangement to actively fund its restoration.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton (rather than snare or tangled_rope) turns on the absence of a concentrated beneficiary capturing extraction: no institutional actor profits from diaspora pluralism's atrophy in the way a captor profits from a snare. The diaspora communal institutions named as agenda_setter are themselves under-resourced and declining, not thriving off the arrangement — they administer what remains of a once-serious institutional model, largely through advocacy and theater rather than functioning infrastructure. This prevents mislabeling the situation as pure extraction (there is a genuine, historically serious coordination function being named, not merely a cover story) while also refusing to certify the reading as a live, functioning Rope — the founding problem status is honestly marked contested, and the founding_problem_corroboration explicitly notes the absence of a source outside both the Zionist establishment and the diasporist advocacy tradition capable of adjudicating whether the underlying problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diasporist_viability_post_1948,
    'Was the diasporist institutional model rendered structurally non-viable by the Holocaust and the establishment of Israel, or does it remain a live alternative that has simply been institutionally starved by resource and legitimacy consolidation around Zionist establishment bodies?',
    'Comparative institutional analysis of surviving autonomist/non-Zionist Jewish communal structures (e.g. in contemporary diaspora communities with active Bundist-descended or minority-rights-oriented organizations) against equivalent historical baselines, assessing whether decline reflects intrinsic non-viability or resource diversion.',
    'If intrinsically non-viable post-1948, the piton classification understates genuine obsolescence (closer to a dead founding problem with no honest remaining coordination function). If merely starved of resources and legitimacy, the piton classification is apt — a real alternative persisting mostly as unfunded advocacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diasporist_viability_post_1948, empirical, 'Whether diasporism''s decline reflects genuine obsolescence or resource/legitimacy starvation.').

omega_variable(
    kernel_framing_indigenous_civic_vs_ancestral,
    'Is the underlying kernel best framed as a civic-political question (which strategy best secures Jewish safety and flourishing: statehood or minority rights?) or as an ancestral-identity question (what does Jewish peoplehood inherently require)? The diasporist reading and the liberal_nationalist_reading both engage the civic-political framing; the religious_covenant_reading and indigenous_return_reading engage a different, ancestral/theological framing that this reading does not directly contest.',
    'No empirical resolution is possible; this is a conceptual framing choice made by the story author based on which sibling readings the diasporist claim is in live argumentative contact with in contemporary discourse (predominantly the liberal_nationalist_reading, secondarily the settler_colonial_reading via shared political-strategic vocabulary).',
    'Under the civic-political framing (adopted here), the diasporist reading directly contests the liberal_nationalist_reading and coexists at a greater conceptual distance from the religious_covenant_reading. Under an ancestral-identity framing, the relationships would shift, and the diasporist reading might be read as more directly foreclosing the religious_covenant_reading''s theological necessity claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_indigenous_civic_vs_ancestral, conceptual, 'Alternative framing of the kernel as civic-strategic versus ancestral-theological, and its effect on which sibling relations are foreclosure versus coexistence.').

omega_variable(
    coercion_versus_consensus_formation,
    'Is the diminished institutional standing of the diasporist position within contemporary Jewish communal life the product of active suppression (deplatforming, funding withdrawal, communal sanction) or the product of a genuine, non-coerced shift in majority Jewish opinion following the Holocaust and Israel''s establishment?',
    'Survey and historical-institutional research distinguishing cases where diasporist/non-Zionist advocates report direct communal sanction from cases of simple demographic and generational preference shift toward Zionist-aligned identity absent any sanctioning mechanism.',
    'If predominantly coercive, the suppression metric (0.55) and the victim declarations for anti-Zionist dissenters are underweighted relative to reality. If predominantly a genuine consensus shift, suppression is overweighted and the reading''s persistence is closer to a minority normative position than an actively suppressed one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_versus_consensus_formation, empirical, 'Whether diasporism''s marginalization is coercive suppression or non-coerced consensus shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_self_determination__diasporist_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t1897, observed).
narrative_ontology:measurement(jewi_tr_t1939, jewish_self_determination__diasporist_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t1939, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1975, jewish_self_determination__diasporist_reading, theater_ratio, 1975, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t1975, observed).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.53).
narrative_ontology:measurement_basis(jewi_tr_t2000, observed).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.62).
narrative_ontology:measurement_basis(jewi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_self_determination__diasporist_reading, base_extractiveness, 1897, 0.2).
narrative_ontology:measurement_basis(jewi_be_t1897, observed).
narrative_ontology:measurement(jewi_be_t1939, jewish_self_determination__diasporist_reading, base_extractiveness, 1939, 0.28).
narrative_ontology:measurement_basis(jewi_be_t1939, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1975, jewish_self_determination__diasporist_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement_basis(jewi_be_t1975, observed).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement_basis(jewi_be_t2000, observed).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.48).
narrative_ontology:measurement_basis(jewi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_self_determination__diasporist_reading, suppression_requirement, 1897, 0.15).
narrative_ontology:measurement_basis(jewi_su_t1897, observed).
narrative_ontology:measurement(jewi_su_t1939, jewish_self_determination__diasporist_reading, suppression_requirement, 1939, 0.2).
narrative_ontology:measurement_basis(jewi_su_t1939, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.32).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1975, jewish_self_determination__diasporist_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement_basis(jewi_su_t1975, observed).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement_basis(jewi_su_t2000, observed).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__diasporist_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(jewi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.1).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language concept 'Jewish self-determination' / 'the Zionism question' per the ε-invariance principle. Each sibling reading (diasporist, liberal_nationalist, indigenous_return, settler_colonial, religious_covenant) instantiates a structurally distinct claim with its own epsilon, beneficiary/victim structure, and claimed type, and none is privileged as the 'real' referent of the natural-language label. The diasporist reading here claims piton status (atrophied genuine alternative) with moderate epsilon; the liberal_nationalist_reading is expected to claim a different type and epsilon; the settler_colonial_reading is expected to claim substantially higher epsilon with a distinct victim set (Palestinians, not diaspora Jews); the indigenous_return_reading and religious_covenant_reading ground legitimacy in different, non-civic-political premises entirely. All five are linked bidirectionally as a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
