% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Reformist Spiritual Reading of the Vedic Corpus (No Prescriptive Social Content)
 *   domain: religious studies / social stratification / hermeneutics
 *
 * SUMMARY:
 *   From the Bengal Renaissance onward, reformist interpreters (Rammohun Roy,
 *   Dayananda Saraswati, Vivekananda, Gandhi's reading of the Gita) taught
 *   that the Vedic hymns are meditations on spiritual unity and metaphorical
 *   cosmology carrying no binding social prescription: varna, on this
 *   reading, is either symbolic psychology or a later corruption foreign to
 *   the revealed core. This interpretive arrangement solved a live legitimacy
 *   problem — how to sustain a scripture-centered community under egalitarian
 *   and missionary critique without surrendering the canon — and it did so
 *   with essentially no coercive machinery: adoption is voluntary, exits are
 *   open, and rival readings remain fully readable. This file authors ONLY
 *   this reading of the contested kernel vedic_corpus_social_prescription
 *   (Rule 1): the orthodox literal-varna reading and the colonial orientalist
 *   codification are separate constraint stories with their own epsilon
 *   values and victim structures, linked through network.affects_constraints.
 *   Epsilon's referent is this reading's own standing interpretive
 *   arrangement, assessed by its own lights; the low value is a
 *   reading-indexed fact, not a verdict on the siblings. The claimed type
 *   (rope) and the authored metrics are independent facts: the metrics record
 *   low extraction with visible drift toward apologetic use.
 *
 * KEY AGENTS:
 *   - - universalist_reform_movements: Agenda-setting beneficiary (organized/mobile) — administers the interpretive standard and collects institutional legitimacy from it
 *   - - dalit_and_shudra_seeker_communities: Primary subsidized party (powerless/constrained) — receives initiation access and textual dignity the birth-ranked order denied
 *   - - diaspora_hindu_households: Secondary beneficiary (moderate/mobile) — carries a portable, non-hierarchical tradition across borders
 *   - - orthodox_varna_lineages: Opposed incumbent authority (institutional/identity_locked) — excluded from setting interpretive terms; mounts sustained resistance
 *   - - vedic_philology_scholarship: Analytical observer (analytical/analytical) — tests the textual claims passage by passage
 *   - - caste_egalitarian_activists: Instrumental beneficiary with partial outsider position (organized/mobile) — borrows the argument while contesting its erasures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.05).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of the Vedic Corpus (No Prescriptive Social Content)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious studies / social stratification / hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '675de99e-df5e-48bd-8f5e-5113b30748ab').
narrative_ontology:cs_kernel_codification('675de99e-df5e-48bd-8f5e-5113b30748ab', fixed_text).
narrative_ontology:cs_authority_grounding('675de99e-df5e-48bd-8f5e-5113b30748ab', lineage).
narrative_ontology:cs_interpretation_layer_present('675de99e-df5e-48bd-8f5e-5113b30748ab').
narrative_ontology:cs_reading_relation('675de99e-df5e-48bd-8f5e-5113b30748ab', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('675de99e-df5e-48bd-8f5e-5113b30748ab', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('675de99e-df5e-48bd-8f5e-5113b30748ab', foundational, shruti_core_free_of_social_prescription).
narrative_ontology:cs_axiom_status(shruti_core_free_of_social_prescription, holdable).
narrative_ontology:cs_axiom_grounding('675de99e-df5e-48bd-8f5e-5113b30748ab', shruti_core_free_of_social_prescription, empirically_contingent).
narrative_ontology:cs_axiom('675de99e-df5e-48bd-8f5e-5113b30748ab', foundational, unity_of_self_grounds_equal_standing).
narrative_ontology:cs_axiom_status(unity_of_self_grounds_equal_standing, holdable).
narrative_ontology:cs_axiom_grounding('675de99e-df5e-48bd-8f5e-5113b30748ab', unity_of_self_grounds_equal_standing, theological).
narrative_ontology:cs_axiom('675de99e-df5e-48bd-8f5e-5113b30748ab', secondary, initiation_open_regardless_of_birth).
narrative_ontology:cs_axiom_status(initiation_open_regardless_of_birth, holdable).
narrative_ontology:cs_axiom_grounding('675de99e-df5e-48bd-8f5e-5113b30748ab', initiation_open_regardless_of_birth, conventional).
narrative_ontology:cs_reference_frame('675de99e-df5e-48bd-8f5e-5113b30748ab', spiritual_unity_metaphorical_cosmology).
narrative_ontology:cs_drift_state('675de99e-df5e-48bd-8f5e-5113b30748ab', contemporary_apologetic_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('675de99e-df5e-48bd-8f5e-5113b30748ab', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, universalist_reform_movements).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, dalit_and_shudra_seeker_communities).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, diaspora_hindu_households).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, caste_egalitarian_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish vernacular translations and commentaries presenting the hymns as meditations on the unity of self and cosmos; run schools, ordain teachers, and admit members of every birth rank to initiation and congregational office. Organizations such as the Brahmo Samaj, Arya Samaj, and Ramakrishna Mission were founded on this way of reading the corpus, and their standing as modern Hinduism depends on keeping the social question outside scripture. Abandoning the reading would dissolve their founding charters.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, universalist_reform_movements, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__reformist_spiritual_reading, universalist_reform_movements, beneficiary).

% Enter reformist congregations to study scripture, receive initiation rites previously restricted by birth, and cite the hymns as warrant for their own dignity. Outside the congregation, caste society continues to treat them much as before, so participation supplements rather than replaces everyday struggle; withdrawing back into birth-ranked village life remains costly.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, dalit_and_shudra_seeker_communities, beneficiary,
    powerless, biographical, constrained, national).

% Practice a portable, temple-friendly tradition abroad that carries no birth-rank obligations into immigration, workplace, and interfaith settings; the reading lets parents transmit the tradition to children without defending hereditary hierarchy. Their attachment is chosen and reversible — they took the tradition up in migration and could set it down.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, diaspora_hindu_households, beneficiary,
    moderate, biographical, mobile, global).

% Hereditary teaching lines and mathas whose authority rests on birth-graded ritual entitlement and on reading the corpus as ordering society. They publish rebuttals, defend traditional initiation rules, and contest reformist translations, but the reformist conversation proceeds largely in English-language print and universalist idiom where their objections arrive pre-dismissed as priestly self-interest. Renouncing their reading would dissolve the status their institutions exist to transmit, so they cannot simply walk away from the dispute.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_lineages, excluded,
    institutional, generational, identity_locked, continental).

% Academic Indologists who date strata, edit manuscripts, and test claims like 'the revealed core lacks social prescription' against passages such as the purusha-sukta and the progressively normative brahmana material. They hold no stake in congregational outcomes and can adopt, amend, or reject the reading passage by passage.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_philology_scholarship, observer,
    analytical, civilizational, analytical, global).

% Ambedkarite and anti-caste organizers who deploy the reading tactically — 'your own hymns never ranked us' — while separately arguing that locating caste wholly outside scripture erases how ritual authority still funds everyday discrimination. Reformist institutions have often kept them at arm's length, so they borrow the argument while sitting partly outside the conversation that produces it.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, caste_egalitarian_activists, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__reformist_spiritual_reading, caste_egalitarian_activists, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, universalist_reform_movements).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a trans-caste, exportable scripture-centered community: one interpretive standard under which converts, lower-caste seekers, and diaspora households share a common canon, common initiation, and common congregational life without inheriting birth-rank obligations — a membership problem that birth-ranked orthodoxy structurally could not solve.
% TRANSFER_FUNCTION: Moves interpretive authority away from birth-qualified brahmin specialists toward vernacular-publishing reform institutions and individual practitioners; moves admission rights (study, initiation, office) to groups the ranked order barred; transfers legitimacy from varna-ranked lineages to universalist ones. Nothing material is taken from anyone — the transfer is of standing, not goods.
% ABSENT_VOICES: Orthodox varna-affirming acharyas and Dharmashastra pandits object continuously but are routed around rather than engaged: reformist fora treat their position as priestly self-interest rather than as a rival hermeneutic, and the decisive conversations happened in English print and universalist idioms they did not control. Ambedkarite critics of the erasure move sit partly outside reformist institutions as well. Both voices exist and are audible; neither helps set the terms inside the arrangement.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the reform movements lose their founding charter and much of their reason to exist; seekers lose the admission path and the textual dignity argument; diaspora communities lose the portable non-hierarchical form they actually practice; orthodox lineages regain uncontested terrain over the corpus; and egalitarian politics loses one of its standard rhetorical resources. Arrangements across religious life visibly depend on it.
% FOUNDING_PROBLEM: Built to reconcile inherited scripture with emerging egalitarian commitments under colonial modernity: missionaries and universalist critics charged that Hindu revelation sanctifies caste, and reformers needed a reading of the canon that preserved its authority while stripping the charge — the legitimacy problem of reform-era Hinduism.
% FOUNDING_PROBLEM_CORROBORATION: Attestation comes from outside the beneficiary set: Vedic philology corroborates that the underlying textual question remains unresolved (stratigraphic and purusha-sukta debates continue in peer-reviewed Indology); orthodox pandit institutions attest from opposition — they deny the founding problem was ever genuine, which itself corroborates that its status is disputed rather than settled; Indian constitutional jurisprudence still adjudicates scripture-versus-equality cases, evidencing that the reconciliation problem persists in live form. No sole attester sits among the reading's beneficiaries.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 terminal) because the arrangement transfers nothing by mechanism: admission, study, and office open voluntarily, and no seat pays a recurring toll to another. Suppression is minimal (0.05) — there is no enforcement infrastructure, no exit barrier beyond ordinary affiliation costs, and rival readings stay fully accessible; suppression is stable across the interval, so per the static-enforcement rule no suppression_requirement series is authored and the scalar carries the picture. Accessibility_collapse is low (0.18): understanding this reading collapses nothing — the orthodox and orientalist readings remain live alternatives, which is precisely why resistance is comparatively high (0.60): orthodox varna lineages and Dharmashastra pandits contest the reading continuously, and egalitarian activists attack its erasures from the other flank. Theater_ratio (0.32 terminal) tracks a real compositional shift: early usage was almost wholly functional (translation, teaching, admission), while late-interval usage increasingly performs defense — citing the reading to deflect caste accountability rather than to organize practice — yet remains below the Goodhart threshold. Both series run on one shared eight-point grid (1828–2024) with every tracked metric authored at every point; all points are observed history. Receipt surface: the arrangement's principal benefits (institutional legitimacy, membership, continuity of charter) demonstrably accrue to the universalist reform institutions themselves, so gain_flow names that seat; fixing_cost is cheap because removal requires no structural dismantling — adherence is voluntary and nothing locks participants in.
 *
 * PERSPECTIVAL GAP:
 *   The seeker and diaspora seats compute something close to a pure coordination good: the reading admits them, dignifies them, travels with them. The orthodox incumbent seat computes dispossession — a hermeneutic monopoly over the corpus lost to vernacular translators and universalist preachers, a real cost that never appears on the beneficiaries' ledger. The analytical seat sees what neither partisan account prices: the erasure externality, where the reading's success lowers the cost of denying caste's entanglement with religious authority while practice persists. These divergences are computed per-seat by the engine from the structural data; the claimed type adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   All four declared beneficiary groups sit near the subsidy end of directionality: seekers receive admission and textual standing, diaspora households receive portability, activists receive an argumentative resource, and the reform movements — though agenda-setters — collect more legitimacy than they spend maintaining the standard, leaving them net-near-beneficiary. No group is declared a victim because the arrangement takes nothing by mechanism; consequently no seat derives near full-target directionality and effective extraction stays near the floor. The orthodox incumbents are authored as excluded rather than paying: the reading does not extract from them through any enforcement channel — it outcompetes them in a market of readings they remain free to keep teaching — so their loss registers as resistance (0.60) and as an absent voice, not as engineered extraction. No directionality overrides are needed: the derivation from beneficiary declarations plus exit options reproduces these positions without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification disciplines both directions of error. Against the snare mislabel: critics allege the no-social-content reading is a cover story for caste interests; the structural data refutes the snare gates — no coercion, no suppressed exits, no suppressed alternatives, no concentrated rent collection — so the cover-story charge cannot bind at the structural level, whatever rhetorical uses occur (those are routed to the appropriation omega). Against premature obsolescence: if the founding reconciliation problem were dead and the institutions merely performing, theater would exceed 0.5 and the arrangement would decay toward piton; instead the founding problem remains contested-live (scripture-versus-equality disputes persist in courts and politics), teaching and admission functions remain active, and the measured theater drift is documented and capped below threshold. The omega variables hold the two live threats — textual counter-evidence forcing defensive maintenance, and apologetic appropriation splitting off a harder derivative constraint — as resolvable questions rather than baked-in verdicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the reformist_spiritual_reading of the kernel vedic_corpus_social_prescription; what structural changes would the sibling readings (orthodox_varna_reading, colonial_orientalist_reading) introduce?',
    'Cross-file comparison of the sibling constraint stories: the orthodox reading declares an enforced birth-rank hierarchy with a victim set; the colonial reading declares a codified administrative law apparatus with its own extraction. Classification is valid only per-reading.',
    'Under the orthodox sibling the same corpus yields a high-extraction enforced hierarchy with identifiable victims; under the colonial sibling it yields administrative extraction. Neither structure belongs in this file — folding them in would break epsilon invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading contested kernel.').

omega_variable(
    textual_strata_adjudication,
    'Is the claim ''no prescriptive social content in the revealed core'' philologically sustainable against the purusha-sukta (RV 10.90) and the progressively normative later Vedic strata, or does the reading survive only by classifying inconvenient passages as interpolation, metaphor, or later accretion?',
    'Peer-reviewed Indology: stratigraphic dating, manuscript testimony, and comparative analysis of sauhita versus brahmana and dharmasutra layers adjudicating where prescription enters the corpus.',
    'If the prescriptive content is original and central rather than marginal, maintaining the reading requires active interpretive defense, pushing the constraint toward tangled_rope (coordinated spiritual community funded by defended textual revisionism) with orthodox textual authorities as the paying seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_strata_adjudication, empirical, 'Whether the no-prescription claim holds at the level of textual evidence.').

omega_variable(
    erasure_deniability_externality,
    'Does the reading''s success reduce lived caste discrimination, or does it supply a deniability shield (''our scriptures never sanctioned caste'') that blunts accountability while caste practice persists untouched?',
    'Outcome studies correlating scriptural-deniability discourse with anti-caste policy support, congregational integration outcomes, and litigation strategy in equality cases.',
    'If the shield effect dominates, elite users of the reading impose an unaccounted cost on those still living under caste — a latent extraction the low authored epsilon does not capture — and per-seat classification for apologetic users should be recomputed upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_deniability_externality, preference, 'Whether the erasure side-effect constitutes attributable harm under this reading''s own lights.').

omega_variable(
    apologetic_appropriation_boundary,
    'Is the late-interval political deployment of the reading (deflecting caste critique, respectability politics) a degeneration of this constraint or a distinct derivative constraint deserving its own file?',
    'Function-retention test: examine whether deployed instances retain the spiritual-practice and admission-coordination function alongside apologetic use; persistent apologetic-only deployment indicates separation.',
    'If a distinct constraint has separated, this story''s rising theater series understates the derivative form''s extraction; a new family member should be authored and linked, leaving this reading''s epsilon low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apologetic_appropriation_boundary, empirical, 'Boundary between this reading and its political-apologetic derivatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1828, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcsp_reformist_tr_t1828, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1828, 0.05).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t1828, observed).
narrative_ontology:measurement(vcsp_reformist_tr_t1858, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1858, 0.08).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t1858, observed).
narrative_ontology:measurement(vcsp_reformist_tr_t1888, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1888, 0.1).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t1888, observed).
narrative_ontology:measurement(vcsp_reformist_tr_t1918, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1918, 0.14).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t1918, observed).
narrative_ontology:measurement(vcsp_reformist_tr_t1947, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1947, 0.18).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t1947, observed).
narrative_ontology:measurement(vcsp_reformist_tr_t1977, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1977, 0.24).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t1977, observed).
narrative_ontology:measurement(vcsp_reformist_tr_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t2000, observed).
narrative_ontology:measurement(vcsp_reformist_tr_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2024, 0.32).
narrative_ontology:measurement_basis(vcsp_reformist_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(vcsp_reformist_be_t1828, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1828, 0.04).
narrative_ontology:measurement_basis(vcsp_reformist_be_t1828, observed).
narrative_ontology:measurement(vcsp_reformist_be_t1858, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1858, 0.06).
narrative_ontology:measurement_basis(vcsp_reformist_be_t1858, observed).
narrative_ontology:measurement(vcsp_reformist_be_t1888, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1888, 0.07).
narrative_ontology:measurement_basis(vcsp_reformist_be_t1888, observed).
narrative_ontology:measurement(vcsp_reformist_be_t1918, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1918, 0.09).
narrative_ontology:measurement_basis(vcsp_reformist_be_t1918, observed).
narrative_ontology:measurement(vcsp_reformist_be_t1947, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1947, 0.11).
narrative_ontology:measurement_basis(vcsp_reformist_be_t1947, observed).
narrative_ontology:measurement(vcsp_reformist_be_t1977, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1977, 0.13).
narrative_ontology:measurement_basis(vcsp_reformist_be_t1977, observed).
narrative_ontology:measurement(vcsp_reformist_be_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement_basis(vcsp_reformist_be_t2000, observed).
narrative_ontology:measurement(vcsp_reformist_be_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2024, 0.15).
narrative_ontology:measurement_basis(vcsp_reformist_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'what do the Vedas say about caste?' covers three structurally distinct claims that cannot share one epsilon. This reformist story (low epsilon, no victim set, voluntary adoption) links to the orthodox story (enforced birth-rank hierarchy, victims present) and the colonial story (administrative codification extracting through law). Downstream influence runs from this reading outward: its success changed the legitimacy conditions under which the orthodox reading is defended and under which colonial codification was ultimately abandoned, which is why the edges point at both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
