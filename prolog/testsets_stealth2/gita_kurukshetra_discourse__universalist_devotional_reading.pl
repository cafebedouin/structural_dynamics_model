% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Universalist Devotional Reading of the Gita — Bhakti as the Caste-Transcendent Path
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This story authors ONE reading of the Kurukshetra discourse kernel: the
 *   text as teaching that devotion (bhakti) is a complete, birth-independent
 *   path, and that dharma culminates in surrender to divine will rather than
 *   in performance of hereditary social role. The standing arrangement under
 *   contest — the arrangement this story is about, and the sole referent of
 *   epsilon — is the devotional-surrender regime as this reading holds it: a
 *   practice community open to all comers, administered by
 *   devotion-credentialed teachers rather than birth-qualified priests. Its
 *   genuine coordination function is real (shared practice, mutual support,
 *   access without hereditary qualification); its extraction asymmetry is
 *   also real (deference, labor, and wealth concentrate in teacher-lineages,
 *   and hereditary ritual specialists bear the cost of their dissolved
 *   mediating monopoly). The claimed_type is authored independently of the
 *   metrics: I believe the structure is a tangled rope — coordination and
 *   extraction through the same apparatus, held by active enforcement — and
 *   the metrics describe what I take to be descriptively true of its actual
 *   operation. The sibling readings (orthodox_literal_reading,
 *   gandhian_allegorical_reading) are separate constraints with their own
 *   epsilon values and beneficiary structures; nothing about them is averaged
 *   into this file.
 *
 * KEY AGENTS:
 *   - universal_devotee_class: primary beneficiary (moderate/mobile) — gains a claimed unmediated route to liberation regardless of birth
 *   - dalit_and_shudra_seeker_communities: named beneficiary (organized/mobile) — the reading's distinctive access claim lands here
 *   - women_devotees: named beneficiary (moderate/mobile) — eligibility extended beyond the male twice-born qualification
 *   - non_hereditary_devotional_teachers: agenda_setter with secondary beneficiary position (institutional/arbitrage) — administers the reading, receives deference and resources
 *   - hereditary_priestly_intermediaries: primary cost-bearer (institutional/identity_locked) — mediating monopoly dissolved by the reading
 *   - doubting_members_of_devotional_movements: excluded voice (powerless/identity_locked) — internal dissent without standing
 *   - academic_indologists: analytical observer (analytical/analytical) — documents the reading's selectivity and its practice gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.32).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Universalist Devotional Reading of the Gita — Bhakti as the Caste-Transcendent Path").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '1069e076-5a73-49b4-83f3-c3de1df9cebe').
narrative_ontology:cs_kernel_codification('1069e076-5a73-49b4-83f3-c3de1df9cebe', fixed_text).
narrative_ontology:cs_authority_grounding('1069e076-5a73-49b4-83f3-c3de1df9cebe', lineage).
narrative_ontology:cs_interpretation_layer_present('1069e076-5a73-49b4-83f3-c3de1df9cebe').
narrative_ontology:cs_reading_relation('1069e076-5a73-49b4-83f3-c3de1df9cebe', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('1069e076-5a73-49b4-83f3-c3de1df9cebe', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('1069e076-5a73-49b4-83f3-c3de1df9cebe', foundational, grace_unconditioned_by_birth).
narrative_ontology:cs_axiom_status(grace_unconditioned_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('1069e076-5a73-49b4-83f3-c3de1df9cebe', grace_unconditioned_by_birth, theological).
narrative_ontology:cs_axiom('1069e076-5a73-49b4-83f3-c3de1df9cebe', foundational, surrender_supersedes_social_role_duty).
narrative_ontology:cs_axiom_status(surrender_supersedes_social_role_duty, holdable).
narrative_ontology:cs_axiom_grounding('1069e076-5a73-49b4-83f3-c3de1df9cebe', surrender_supersedes_social_role_duty, theological).
narrative_ontology:cs_reference_frame('1069e076-5a73-49b4-83f3-c3de1df9cebe', lineage_transmitted_universal_bhakti).
narrative_ontology:cs_drift_state('1069e076-5a73-49b4-83f3-c3de1df9cebe', contemporary_global_movement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1069e076-5a73-49b4-83f3-c3de1df9cebe', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, dalit_and_shudra_seeker_communities).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, non_hereditary_devotional_teachers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, hereditary_priestly_intermediaries).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_as_independent_path_to_liberation).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, divine_grace_unconditioned_by_birth).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, surrender_as_fulfillment_of_dharma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary practitioners across caste and gender lines who take up the path as taught: chanting, study, service, and surrender of outcomes to the divine. The arrangement grants them a claimed direct route to the text's highest goal requiring no hereditary qualification, no ritual intermediary, and no Sanskrit expertise. Leaving the path carries social and community costs but no formal barrier; many move between teachers and movements freely.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    moderate, biographical, mobile, global).

% Communities historically barred from Vedic study and temple access by birth-status rules. Under this reading they are named explicitly as eligible for the highest attainment — the text's own list of those who take refuge includes them. Temple-entry struggles and devotional-movement participation gave these communities collective organization, and the reading supplies scriptural warrant that their exclusion was never spiritually necessary.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, dalit_and_shudra_seeker_communities, beneficiary,
    organized, generational, mobile, continental).

% Women, named in the text alongside those of disadvantaged birth as fully eligible through refuge-taking. The reading removes the male twice-born qualification for attainment; in practice women teach, found lineages, and lead congregations in movements governed by this reading, though household obligations and institutional leadership ratios remain uneven.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees, beneficiary,
    moderate, biographical, mobile, global).

% Acharyas, gurus, and movement founders who select, translate, and transmit the reading: they decide which verses anchor the curriculum, ordain successors, and administer discipline within their institutions. Their authority rests on devotional realization and disciplic succession rather than birth — precisely what the reading licenses. They receive donations, labor, and deference, and can carry their followings across institutional contexts when disputes split movements.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, non_hereditary_devotional_teachers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, non_hereditary_devotional_teachers, beneficiary).

% Hereditary ritual specialists whose standing, income, and mediating function rest on birth-qualified access to the sacred. The reading declares their qualification spiritually irrelevant: if devotion alone suffices, the exclusive rites they administer lose their salvific necessity. Their authority is fused with the birth-status claim the reading dissolves, so adapting means renouncing the very ground of their position; some retrain as teachers of the text, most defend the older arrangement.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, hereditary_priestly_intermediaries, payer,
    institutional, generational, identity_locked, continental).

% Members raised inside devotional communities who privately question the total-surrender demand, the authority of lineage heads, or the use of donated wealth, but lack standing to voice dissent without risking family ties, marriage prospects, and their entire social world. Their objections surface mainly as exit stories rather than internal debate.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, doubting_members_of_devotional_movements, excluded,
    powerless, biographical, identity_locked, global).

% Scholars of South Asian religions who document the text's layered composition, the history of its competing readings, and the gap between egalitarian doctrine and institutional practice. They attest the reading's selectivity and its consequences from outside any benefiting party.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, academic_indologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, non_hereditary_devotional_teachers).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, birth-independent route to the text's highest goal: one devotional standard — chanting, study, service, surrender — that anyone can adopt, solving the access problem that hereditary ritual qualification otherwise creates and giving geographically scattered seekers a shared practice community.
% TRANSFER_FUNCTION: Moves deference, labor, and wealth from lay devotees upward to teacher-lineages and movement institutions; moves interpretive authority away from hereditary priestly specialists toward devotion-credentialed teachers; moves the status of 'eligible for liberation' from a birth-qualified few to all comers.
% ABSENT_VOICES: Doubting members inside devotional movements have no standing to dissent without losing kin and community. Hereditary ritualists appear in this story only as cost-bearers, not as parties with a say in the arrangement that displaces them. Ambedkarite and secular critics of the text's war-frame sit wholly outside the devotional conversation this reading organizes.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, millions of practitioners would lose their operative account of dharma and of access to the text's goal; teacher-lineage institutions would dissolve; hereditary intermediaries would regain the mediating field by default; and the text's egalitarian strata would lose their institutional carrier. Religious access would reorganize around gatekept or alternative paths rather than staying as it is.
% FOUNDING_PROBLEM: Hereditary gatekeeping of religious access: liberation appeared reserved to twice-born males with ritual qualification, mediated by birth-status specialists. The reading was built, on the text's universalist strata, to dissolve that gate.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: temple-entry movement records and litigation document enforced exclusion from religious space; dalit autobiographical testimony attests the barrier's lived reality; academic indology documents the varna-qualification architecture of pre-modern ritual access; and the orthodox parties' own historical defenses of birth-qualification attest that the gate existed and was defended.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.32): the core function distributes access rather than concentrating it, but deference, labor, and wealth demonstrably flow upward to lineage heads, and the displaced priestly stratum bears a real cost. Suppression (0.28) is authored as a raw structural property — it is NOT scaled by power or scope in the way extractiveness is; it reflects social exit costs, doctrinal dismissal of rival paths as salvifically void, and conformity pressure inside movements. Theater is low (0.15): chanting, study, and service are functional practices, with only a performative fringe around institutional pageantry and membership display. Accessibility_collapse (0.55) is mid-range: within the reading's frame, caste-qualification and ritual monopoly collapse as routes to the text's goal, but the wider pluralism of paths (jñana, other texts, secular ethics) remains available outside it. Resistance (0.50) is moderate: orthodox authorities contested the reading for centuries and continue to, internal dissent surfaces periodically, and secular critics attack the surrender-demand. The temporal series share one grid (1890, 1920, 1950, 1980, 2000, 2026) with every tracked metric authored at every point. All three series are hump-shaped, not cyclic: a reform-era devotional revival with volunteer economics (1890s), institutionalization through the twentieth century, a peak around 1980 when global high-demand movements ran donation economies at maximum guru authority, then partial easing after scandal-driven reforms, financial-transparency pressure, and internet-mediated access reduced dependence on mediators. The suppression_requirement series tracks enforcement-capacity change specifically: disciplinary machinery matured with organizational scale and eased modestly after the reform period.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the hereditary priestly seat, the reading is dispossession: it annuls the birth-status claim on which their entire authority, income, and identity rest, and their identity_lock means they experience the arrangement as an existential attack rather than a reform. From the dalit, women's, and general devotee seats, the same structure operates as subsidy: it hands them an eligibility claim their grandmothers were denied. From the teacher seat it appears as stewardship — they run the apparatus and receive its flows, and their dual agenda_setter/beneficiary position is exactly where the coordination and extraction components fuse. The doubting-member seat experiences suppression the beneficiary seats do not see. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: universal_devotee_class, dalit_and_shudra_seeker_communities, and women_devotees sit near the beneficiary end (damped effective extraction, mobile exits pushing them further toward subsidy). non_hereditary_devotional_teachers derive low d from their beneficiary position, but their agenda_setter role and arbitrage-grade exits mean the engine should treat them as the seat where gains accrue — hence the explicit gain_flow authoring rather than a directionality override. hereditary_priestly_intermediaries derive high d from their victim declaration compounded by identity_locked exit: trapped or identity-locked targets sit nearer the full-target end than mobile ones. No overrides are used because the beneficiary/victim declarations plus exit options already capture every structural relationship; the one relationship the derivation cannot express — that the teacher seat both runs the arrangement and collects from it — is carried by the secondary_role and the receipt surface instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — hereditary gatekeeping of religious access — is still live: caste-based exclusion from religious space persists, so the arrangement has not outlived its function and no mandatrophy is declared. The arrangement is not transitional and correctly carries no sunset clause. The mandatrophy risk to watch is the one named in the gatekeeper_substitution_or_opening omega: if lineage authority merely replaced priestly authority as a new gate, the arrangement's function would be drifting from opening access toward institutional self-perpetuation — the classic path by which a coordination structure atrophies while its forms continue. Theater_ratio is currently low and the temporal series shows no Goodhart-style decoupling, so there is no present signal of piton formation; the omega keeps the question open rather than settling it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_gita_kernel,
    'This constraint instantiates only the universalist_devotional_reading of the gita_kurukshetra_discourse kernel; which structural features of the arrangement are artifacts of this reading rather than of the text, and what would the sibling readings change?',
    'Compile and compare the sibling stories (orthodox_literal_reading, gandhian_allegorical_reading): shifts in victim sets, epsilon, and enforcement structure isolate the reading-indexed components of this arrangement.',
    'Under the orthodox sibling the victim set becomes lower-caste and war-conscripted populations with far higher epsilon; under the gandhian sibling the arrangement contracts to inner discipline with almost no external beneficiary structure. The divergence locates what this reading itself contributes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_gita_kernel, conceptual, 'Committer structure: one reading of a contested kernel; sibling deltas recorded here, not in the constraint body.').

omega_variable(
    surrender_autonomy_extraction,
    'Is the total-surrender demand (relinquishing all dharmas to the divine) a liberation mechanism, or an extraction of autonomous moral judgment that this reading''s own lights cannot register from inside?',
    'Longitudinal comparison of members who exit versus remain: autonomy measures, financial entanglement depth, and post-exit recovery trajectories across movements that govern themselves by this reading.',
    'If surrender functions as judgment-extraction, effective extraction at the member seat rises sharply and the arrangement trends toward the snare end despite its egalitarian doctrine; if not, the measured extraction is mostly institutional overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surrender_autonomy_extraction, empirical, 'Blind-spot test on the reading''s core demand, invisible from the reading''s own seat.').

omega_variable(
    gatekeeper_substitution_or_opening,
    'Did dissolving hereditary gatekeeping open access, or substitute a new gate (teacher-lineage authority) in place of the old one?',
    'Compare access costs under priestly mediation versus lineage mediation: initiation requirements, deference expectations, and bottleneck positions held by non-hereditary teachers across movements.',
    'If substitution, the arrangement is coordination with rotated gatekeepers and the extraction asymmetry is structural rather than transitional; if opening, the teacher stratum''s capture is contingent and reformable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_substitution_or_opening, empirical, 'Whether anti-gatekeeping replaced gatekeeping with a new gate.').

omega_variable(
    varna_strata_weighting,
    'The text contains both universalist strata (refuge-taking open to all, the final surrender injunction) and varna-affirming strata (birth-ordered duties); does this reading''s anti-caste structure survive full-text weighting, or does it depend on selective emphasis?',
    'Philological stratification of the text''s layers plus reception history: track whether harmonizing commentators can bind the strata together without remainder.',
    'If the varna strata are load-bearing, the reading''s coordination function narrows to a selection rather than a reading of the whole, weakening its claim to supersede the orthodox sibling on the text''s own authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_strata_weighting, conceptual, 'Selective-emphasis dependence of the reading''s universalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 1890, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1890, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(gita_tr_t1920, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(gita_tr_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement(gita_tr_t1980, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(gita_tr_t2026, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(gita_be_t1890, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1890, 0.22).
narrative_ontology:measurement(gita_be_t1920, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1920, 0.26).
narrative_ontology:measurement(gita_be_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(gita_be_t1980, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1980, 0.36).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(gita_be_t2026, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2026, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1890, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1890, 0.18).
narrative_ontology:measurement(gita_su_t1920, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1920, 0.22).
narrative_ontology:measurement(gita_su_t1950, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1950, 0.27).
narrative_ontology:measurement(gita_su_t1980, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1980, 0.32).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(gita_su_t2026, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what the Gita teaches' covers three structurally distinct normative regimes with different epsilon values, victim sets, and enforcement structures; per the epsilon-invariance principle they are authored as separate linked stories rather than one story with a measurement parameter. This file instantiates the universalist devotional regime: epsilon is authored for the devotional-surrender arrangement as this reading holds it (moderate-low, concentrated in lineage capture and displaced-priest costs). The orthodox sibling authors high epsilon for the caste-duty-and-war regime it takes the text to establish; the gandhian sibling authors low epsilon for an inner-discipline regime with almost no external beneficiary structure. The upstream/downstream pressure between family members runs through shared verses: whichever reading controls the text's public meaning conditions the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
