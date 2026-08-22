% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Kurukshetra Discourse — Gandhian Allegorical Reading: the Standing Caste-War Arrangement as Conscience Sees It
 *   domain: religious/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Kurukshetra discourse — Krishna's counsel to Arjuna on the field of
 *   the Bhagavad Gita — anchors a standing arrangement in which birth assigns
 *   station, station assigns duty, and the warrior's killing is consecrated
 *   as righteousness when performed as duty. This file is the
 *   Gandhian-authored member of a three-story constraint family sharing that
 *   kernel; it assesses the standing orthodox arrangement through the
 *   allegorical reading, under which the battlefield is the soul's interior
 *   war, the command to fight is a command to master the self, and no birth
 *   carries a divine warrant to rule, serve, or kill. On that reading the
 *   arrangement's epsilon is authored high: hereditary service, ritual
 *   exclusion, and sanctified warfare move labor, deference, and blood from
 *   the many to a priestly-martial few. KEY AGENTS (by structural
 *   relationship): brahminical_interpretive_establishment — agenda-setting
 *   beneficiary (institutional/identity_locked) — administers textual
 *   authority and collects deference and service; kshatriya_martial_elite —
 *   secondary beneficiary (powerful/arbitrage) — receives sanctified violence
 *   and rank; dalit_and_shudra_labouring_castes — primary target
 *   (powerless/trapped) — bears hereditary service extraction;
 *   civilian_populations_of_dharmic_war — secondary target
 *   (powerless/trapped) — bears war's blood cost;
 *   anti_caste_dissent_traditions — excluded voice (organized/constrained);
 *   gandhian_moral_conscience — analytical observer (analytical/analytical) —
 *   the seat this reading speaks from.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.78).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.55).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Kurukshetra Discourse — Gandhian Allegorical Reading: the Standing Caste-War Arrangement as Conscience Sees It").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '74f71fb5-1c49-4afa-bc36-f5f80b165497').
narrative_ontology:cs_kernel_codification('74f71fb5-1c49-4afa-bc36-f5f80b165497', fixed_text).
narrative_ontology:cs_authority_grounding('74f71fb5-1c49-4afa-bc36-f5f80b165497', practice).
narrative_ontology:cs_interpretation_layer_present('74f71fb5-1c49-4afa-bc36-f5f80b165497').
narrative_ontology:cs_reading_relation('74f71fb5-1c49-4afa-bc36-f5f80b165497', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('74f71fb5-1c49-4afa-bc36-f5f80b165497', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('74f71fb5-1c49-4afa-bc36-f5f80b165497', foundational, ahimsa_supreme_principle).
narrative_ontology:cs_axiom_status(ahimsa_supreme_principle, holdable).
narrative_ontology:cs_axiom_grounding('74f71fb5-1c49-4afa-bc36-f5f80b165497', ahimsa_supreme_principle, deontological).
narrative_ontology:cs_axiom('74f71fb5-1c49-4afa-bc36-f5f80b165497', foundational, kurukshetra_inner_battlefield_allegory).
narrative_ontology:cs_axiom_status(kurukshetra_inner_battlefield_allegory, holdable).
narrative_ontology:cs_axiom_grounding('74f71fb5-1c49-4afa-bc36-f5f80b165497', kurukshetra_inner_battlefield_allegory, deontological).
narrative_ontology:cs_reference_frame('74f71fb5-1c49-4afa-bc36-f5f80b165497', inner_battlefield_ahimsa_supremacy).
narrative_ontology:cs_drift_state('74f71fb5-1c49-4afa-bc36-f5f80b165497', post_gandhi_reliteralization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('74f71fb5-1c49-4afa-bc36-f5f80b165497', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_establishment).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_martial_elite).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalit_and_shudra_labouring_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, civilian_populations_of_dharmic_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of the scriptural canon and its authorized interpretation. They adjudicate what dharma requires, preside over ritual life, and receive deference, offerings, and directed service as the price of sacral access. Their social standing is constituted by the arrangement they administer; stepping outside it would mean relinquishing the identity and livelihood the interpreter role carries. Reform currents are absorbed by reinterpreting the canon rather than by surrendering the gatekeeping role.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_establishment, beneficiary).

% Ruling and warrior lineages whose recourse to arms is consecrated as righteous duty when fought under the warrant the texts supply. They convert martial standing into land, tribute, and political authority across successive regimes, and fund the priestly establishment that sanctifies their campaigns. Their position travels well: when a dynasty falls, martial capital reattaches to the next patron.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_martial_elite, beneficiary,
    powerful, generational, arbitrage, regional).

% Hereditary service and laboring communities whose occupations, marriage circles, and village standing are fixed by birth under the arrangement's role doctrine. They render obligatory service to higher-ranked households, endure ritual exclusion, and carry the stigma the purity code assigns them. Leaving means forfeiting livelihood networks and community altogether; the few who exit by conversion pay in dislocation and reprisal.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalit_and_shudra_labouring_castes, payer,
    powerless, biographical, trapped, local).

% Farmers, townspeople, and conscripted foot soldiers who absorb the actual blood cost of wars narrated as righteous duty. They do not author the wars, rarely share the spoils, and their villages stand along the campaign routes. Their objection survives mainly in lament literature rather than in the councils that declare the fighting holy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, civilian_populations_of_dharmic_war, payer,
    powerless, immediate, trapped, regional).

% Renunciant and devotional movements — Buddhist and Jain orders, bhakti poets of low birth, Sikh egalitarianism, and later Ambedkarite organizers — that deny birth-rank and refuse the purity code. Orthodoxy answers them with excommunication, satire, or absorption rather than argument; their texts circulate outside the canonical curriculum they contest.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, anti_caste_dissent_traditions, excluded,
    organized, generational, constrained, continental).

% The seat this reading speaks from: the individual conscience testing every scriptural command against nonviolence before obeying it. It holds no office in the arrangement, collects nothing from it, and reads the war council on the field as a mirror in which each reader faces the war inside. Its leverage is testimony and self-suffering rather than administration.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_moral_conscience, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_establishment).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates a large agrarian society: hereditary division of labor, a shared ritual calendar, dispute arbitration through dharmic authority, transmission of the textual canon, and a mutual-obligation ideology that stabilized cooperation where no centralized enforcement capacity existed.
% TRANSFER_FUNCTION: Moves compulsory service, agricultural and artisanal labor, ritual fees, deference, and military blood from the serving and laboring castes and from war-facing populations upward to the priestly interpreters and martial elites, under the warrant that each party is fulfilling divinely assigned duty.
% ABSENT_VOICES: The laboring castes whose duties the texts enumerate were never seated in the councils that composed and canonized those enumerations; anti-caste dissenters were expelled rather than answered. They stand outside the interpretive circle — their objections survive in rival canons the orthodoxy classifies as heterodox.
% DISAPPEARANCE_RATIONALE: Kinship and marriage markets, village labor exchange, ritual calendars, temple economies, and the political theology of righteous warfare would all lose their coordinating warrant overnight; occupation and inheritance would detach from birth; the interpreter class would lose its gatekeeping income; and wars would have to seek justification outside sanctified duty.
% FOUNDING_PROBLEM: After the Vedic clan order gave way to territorial kingdoms and a diversified agrarian economy, elites needed a stable blueprint for hereditary role allocation, ritual integration, and the legitimation of organized violence — a sacred grammar that made station, service, and soldiering obligations rather than bargains.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: B.R. Ambedkar's 'Annihilation of Caste' (1936) argues the arrangement's integrating function is fully replaceable and its hierarchies indefensible; the colonial administrative record shows taxation, courts, and policing performing the integration function without sacral sanction; and the Indian Constitution's abolition of untouchability demonstrates the society functioning after the arrangement was stripped of legal force. Orthodox defenders dispute all three attestations — that dispute is itself the contest this reading enters.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Interval 0-120 maps approximately 1900-2020 CE. Extractiveness is authored high (0.78 at interval end) because the arrangement moves hereditary labor, ritual fees, deference, and war's blood upward under a divine-warrant grammar the allegorical reading denies; the series dips mid-century as constitutional abolition of untouchability, land reform, and reservation policy strip formal enforcement, then partially rebounds as exclusion re-forms through economic and political channels. Suppression falls from 0.88 to 0.55: legal enforcement collapsed after 1950, leaving social enforcement — ostracism, matrimonial policing, atrocity — carrying a reduced but real load. Theater rises steadily to 0.50 as the arrangement's stated function (divinely ordered integration) decays while ritual and rank maintenance continue — a growing share of activity defends the arrangement rather than performs it. Accessibility_collapse sits at 0.55: renunciation, bhakti, conversion, and constitutional citizenship are real exits, each priced. Resistance 0.62 reflects an unbroken dissent lineage from the Buddha through Kabir and Ravidas to Ambedkar; coalition formation among the powerless (Ambedkarite organization, reservation politics) is precisely what bent the extraction curve mid-century, and its partial character explains why the bend was incomplete. Coordination type is identity_coordination: the arrangement's surviving coordination function is boundary and role maintenance — membership, marriage circles, ritual standing — rather than resource allocation proper. Claim and metrics are independent: the tangled_rope claim concedes the arrangement's residual coordination (ritual integration, textual transmission, dispute arbitration) while the metrics record its operating asymmetry; the engine computes each seat's type from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the interpreter's seat the arrangement is sacred order: the interpreter experiences obligation received and conferred, not imposition, and computes near the beneficiary end with negligible effective extraction. From the trapped payer seats the same verses read as a cage with a liturgy — hereditary service, exclusion, and conscripted blood with no priced exit. The martial seat experiences sanctification, its violence laundered as duty, and computes low extraction despite bearing battlefield risk it has internalized as honor. The engine derives these divergent classifications from the declared directionalities; the Gandhian reading predicts them: conscience sees what station cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation. The interpretive establishment (institutional power, identity_locked exit) sits near the full-beneficiary end — the arrangement subsidizes it, damping effective extraction toward subsidy. The martial elite (powerful, arbitrage exit) sits similarly low; its battlefield exposure is self-chosen under the sanctification the arrangement provides. The trapped payer seats sit near the full-target end: hereditary service castes and war-facing civilians bear the transfer with no priced exit, amplifying effective extraction. Continental spatial scope raises verification difficulty, scaling effective extraction modestly upward for the targets. No directionality overrides were needed: beneficiary/victim declarations plus exit atoms already place every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating a post-clan agrarian order and legitimating organized violence without bargaining — is dead: states, markets, and constitutional law perform the integration, and the society demonstrably functions after the arrangement lost legal force. Yet the arrangement persists on inertia, identity fusion (the interpreter class has become its function; exit would dissolve the self-concept the role carries), and rising theatrical maintenance — theater_ratio crossing 0.50 marks proxy performance replacing function. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) flags exactly the zombie condition this reading diagnoses: a coordination shell kept upright by performance around a still-live extraction core. Mandatrophy resolution prevents two errors: reading the arrangement as pure extraction erases the real coordination it once supplied and partially still supplies; reading it as pure coordination launders the asymmetry the metrics record. mandatrophy_resolved is declared: the mandate has outlived its function even while its extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the gita_kurukshetra_discourse kernel: which reading correctly specifies what the discourse mandates, and does the standing orthodox arrangement retain scriptural warrant once the allegorical turn is granted?',
    'Philological stratification of the text plus reception history: if the battlefield-frame verses resist allegorization on internal evidence, the orthodox warrant strengthens; if the allegorical reading tracks earlier or deeper strata of the discourse, the warrant collapses.',
    'If the allegorical reading prevails, the orthodox arrangement loses scriptural warrant and drifts toward inertial, theatrical persistence; if the literal reading prevails, high extraction stands with active enforcement and the victim set widens to include conscientious objectors to sanctified war.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Sibling readings of the same kernel would change the victim set, epsilon, and enforcement profile of this constraint.').

omega_variable(
    divine_ordination_naturalness,
    'Is the caste-war hierarchy an emergent organic order, as its defenders claim, or an enforced construction maintained by identifiable beneficiaries?',
    'Comparative history of complex agrarian societies achieving integration and war-legitimation without birth-rank doctrine, combined with enforcement records (ostracism, atrocity, matrimonial policing) showing the maintenance costs the order requires.',
    'If constructed with beneficiaries, the natural-order defense fails and the arrangement reads as maintained extraction rather than discovered order, shifting classification toward pure extraction; if genuinely emergent, part of the measured burden is the irreducible cost of social order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_ordination_naturalness, empirical, 'Natural-law versus constructed-constraint ambiguity in the orthodox arrangement''s central claim.').

omega_variable(
    allegorical_fidelity,
    'Is the Gandhian allegorical reading a faithful recovery of the discourse''s intent, or a strategic pacifist projection onto an unwilling text?',
    'Philological analysis of whether the war setting is load-bearing or removable in the discourse''s narrative architecture, together with the pre-modern allegorical commentarial precedent available to the tradition.',
    'If projection, the Gandhian critique loses evidentiary force against the orthodox warrant even where its moral conclusions stand independently; if faithful, the orthodox arrangement rests on misreading and this reading''s epsilon assessment gains interpretive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_fidelity, conceptual, 'Whether the reading that authors this story is itself hermeneutically sound.').

omega_variable(
    structural_internalized_suppression,
    'How much of the arrangement''s contemporary suppressive force is structural (economic dependency, village and matrimonial exclusion) versus internalized (karma-theodicy, purity shame that makes acceptance feel virtuous)?',
    'Post-reform cohort comparison: if compliance persists where enforcement capacity has been removed, internalization carries the load; if compliance tracks enforcement budgets and patronage networks, structure dominates.',
    'If internalized, effective suppression exceeds the structural measure and will outlast legal reform, flattening the declining suppression series; if structural, continued enforcement removal compounds the decline and accelerates the drift toward theatrical persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_internalized_suppression, empirical, 'Structural versus internalized split in the remaining suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_gandhian_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gita_gandhian_tr_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(gita_gandhian_tr_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(gita_gandhian_tr_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(gita_gandhian_tr_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(gita_gandhian_tr_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 100, 0.44).
narrative_ontology:measurement(gita_gandhian_tr_t120, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 120, 0.5).

% Extraction over time
narrative_ontology:measurement(gita_gandhian_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(gita_gandhian_be_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 20, 0.86).
narrative_ontology:measurement(gita_gandhian_be_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(gita_gandhian_be_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(gita_gandhian_be_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(gita_gandhian_be_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 100, 0.75).
narrative_ontology:measurement(gita_gandhian_be_t120, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 120, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gita_gandhian_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(gita_gandhian_su_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(gita_gandhian_su_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(gita_gandhian_su_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(gita_gandhian_su_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(gita_gandhian_su_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(gita_gandhian_su_t120, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 120, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what does the Gita teach?' decomposes into three structurally distinct constraint stories sharing one kernel (gita_kurukshetra_discourse): this Gandhian allegorical file, the orthodox literal file, and the universalist devotional file. Each authors its own epsilon over the shared referent — the standing orthodox arrangement — per the epsilon-invariance principle; the values differ because the readings differ, not because the topic does. The orthodox file carries the deepest historical entrenchment and stands upstream as the arrangement the other two contest; this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
