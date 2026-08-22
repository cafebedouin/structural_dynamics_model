% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Constitutional Equality Conformity Requirement on Dharmic Interpretation (Reformist Reading)
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   Since the republic's founding (T=0 corresponds to 1950, T=75 to 2025),
 *   the operative settlement requires that the dharmic corpus be received
 *   only through equality-conforming interpretation: textual meaning is
 *   answerable to constitutional principles, caste hierarchy is officially
 *   classified as historical accretion rather than scriptural essence, and
 *   rational-constitutional critique displaces birth-lineage authority as the
 *   arbiter of what the tradition says. The settlement is enforced through
 *   courts (temple-entry litigation, essential-practices doctrine), enabling
 *   legislation, and a delegitimation economy in which lineage-based
 *   interpretive claims carry no public standing. This story is ONE member of
 *   a three-story constraint family decomposing the contested kernel
 *   'vedic_dharmic_corpus': the hereditary_monopoly_reading (birth-derived
 *   authority, divinely ordained varna — high extraction, priestly
 *   beneficiaries), the bhakti_devotional_reading (devotion over birth,
 *   community-enforced — low state entanglement), and this reformist reading
 *   (moderate extraction, inverted beneficiary structure, heavy state
 *   entanglement). Per the epsilon-invariance principle the readings are
 *   separate files with separate epsilon values, linked by network edges; the
 *   upstream hereditary reading is the arrangement this reading contests and
 *   cites as its justification-for-existence.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - indian_constitutional_state: Agenda-setter and receipt-holder (institutional/identity_locked) — administers the conformity requirement through courts and legislation; collects adjudicative jurisdiction and legitimating narrative
 *   - - dalit_rights_movements: Primary beneficiary (organized/constrained) — receives legal instruments, legitimation, and access gains; cannot exit the society they are transforming
 *   - - reformist_scholarly_establishment: Beneficiary (institutional/identity_locked) — judges, academics, and movement intellectuals whose standing and careers are constituted by the reading
 *   - - hereditary_priestly_lineages: Primary target (moderate/identity_locked) — bear delegitimation and eroding ritual income; their birth-lineage identity is the thing the reading disqualifies
 *   - - orthodox_mutt_establishments: Target (institutional/constrained) — institutional custodians who lose exclusive interpretive jurisdiction and litigate or accommodate
 *   - - custom_observant_ritual_communities: Secondary target (moderate/constrained) — communities whose ritual life is subjected to external adjudication while receiving partial civic benefits
 *   - - village_non_brahmin_ritual_specialists: Excluded voice (powerless/trapped) — folk practitioners unrecognized by both the Sanskritic establishment and the constitutional frame
 *   - - comparative_religion_scholars: Analytical observer (analytical/analytical) — sees the full three-reading structure from outside the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.58).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Constitutional Equality Conformity Requirement on Dharmic Interpretation (Reformist Reading)").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '84bf97f3-b9b2-4bf1-a06d-5924948733ef').
narrative_ontology:cs_kernel_codification('84bf97f3-b9b2-4bf1-a06d-5924948733ef', distributed).
narrative_ontology:cs_authority_grounding('84bf97f3-b9b2-4bf1-a06d-5924948733ef', lineage).
narrative_ontology:cs_interpretation_layer_present('84bf97f3-b9b2-4bf1-a06d-5924948733ef').
narrative_ontology:cs_reading_relation('84bf97f3-b9b2-4bf1-a06d-5924948733ef', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('84bf97f3-b9b2-4bf1-a06d-5924948733ef', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('84bf97f3-b9b2-4bf1-a06d-5924948733ef', foundational, constitutional_equality_adjudicates_textual_meaning).
narrative_ontology:cs_axiom_status(constitutional_equality_adjudicates_textual_meaning, holdable).
narrative_ontology:cs_axiom_grounding('84bf97f3-b9b2-4bf1-a06d-5924948733ef', constitutional_equality_adjudicates_textual_meaning, conventional).
narrative_ontology:cs_axiom('84bf97f3-b9b2-4bf1-a06d-5924948733ef', foundational, caste_hierarchy_is_historical_accretion).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion, holdable).
narrative_ontology:cs_axiom_grounding('84bf97f3-b9b2-4bf1-a06d-5924948733ef', caste_hierarchy_is_historical_accretion, empirically_contingent).
narrative_ontology:cs_axiom('84bf97f3-b9b2-4bf1-a06d-5924948733ef', secondary, rational_critique_supersedes_lineage_authority).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_lineage_authority, holdable).
narrative_ontology:cs_axiom_grounding('84bf97f3-b9b2-4bf1-a06d-5924948733ef', rational_critique_supersedes_lineage_authority, instrumental).
narrative_ontology:cs_reference_frame('84bf97f3-b9b2-4bf1-a06d-5924948733ef', constitutional_equality_baseline).
narrative_ontology:cs_drift_state('84bf97f3-b9b2-4bf1-a06d-5924948733ef', contemporary_india, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('84bf97f3-b9b2-4bf1-a06d-5924948733ef', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_scholarly_establishment).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_constitutional_state).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priestly_lineages).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_mutt_establishments).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, custom_observant_ritual_communities).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, annihilation_of_caste_program).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, historical_accretion_hermeneutics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the conformity requirement through its courts, legislature, and enforcement agencies: decides which practices are essential, orders temple access, prosecutes caste atrocity, and certifies which readings of the corpus carry public standing. Collects adjudicative jurisdiction and the legitimating narrative of constitutional transformation. Cannot abandon the settlement without repudiating its own founding charter — the republic's self-concept is constituted through this transformation of its religious inheritance.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_constitutional_state, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_constitutional_state, beneficiary).

% Mobilized Ambedkarite and Dalit organizations that invoke the settlement to claim temple access, legal protection, and equal standing inside the tradition's institutions. The settlement validates their reading of the corpus and arms them with litigation and statutory tools. They cannot exit the society they are transforming; their leverage depends on the state apparatus remaining committed to the settlement.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements, beneficiary,
    organized, generational, constrained, national).

% Judges, constitutional lawyers, historians, philologists, and movement intellectuals who staff the interpretive apparatus: they supply the expert testimony, historical scholarship, and doctrinal reasoning on which conformity rulings rest. Careers, reputations, and professional self-concept are built on the reading's premises; pivoting to defend the rival reading would forfeit their standing entirely.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_scholarly_establishment, beneficiary,
    institutional, biographical, identity_locked, national).

% Families whose ritual office and interpretive standing descend through birthline. Under the settlement their claims carry no public weight: ritual incomes erode, temples pass under state or reformist control, and their account of the texts is officially classified as accretion-serving-interest rather than essence. Exit would mean dissolving the lineage identity that constitutes who they are; they bear the settlement's deepest identity-level costs.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priestly_lineages, payer,
    moderate, generational, identity_locked, regional).

% Large monastic institutions and peethas that custodian traditional interpretation, command substantial resources, and retain mass followings. They lose exclusive interpretive jurisdiction, submit to litigation and regulatory oversight of their religious administration, and absorb the delegitimation of their public teaching office — while retaining enough resources to litigate, accommodate, and fund counter-mobilization rather than simply comply.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_mutt_establishments, payer,
    institutional, generational, constrained, national).

% Middle-caste and locality communities whose festival calendars, marriage customs, and temple practices are now answerable to external adjudication. They bear compliance costs and the stigma of officially suspect custom, while also receiving partial benefits from the shared civic order the settlement maintains — a mixed position that leaves them ambivalent participants rather than full opponents.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, custom_observant_ritual_communities, payer,
    moderate, biographical, constrained, regional).

% Folk priests, ojhas, and village ritual functionaries whose authority never fit Sanskritic orthodoxy and finds no place in the constitutional frame either. Both contending establishments treat their practices as noise: the orthodox side as impurity, the reformist side as superstition pending reform. They would object that the entire contest proceeds over their heads, but they have no seat in the courtroom-academic circuit where meaning is decided.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, village_non_brahmin_ritual_specialists, excluded,
    powerless, biographical, trapped, local).

% Academic students of South Asian religion and law, inside and outside India, who track the three-reading contest comparatively: how the settlement evolved, what the rival readings cost their holders, and how comparable societies handle scriptural inheritance under equality guarantees. They collect no returns from the settlement and bear none of its burdens.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_constitutional_state).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive baseline that lets a multi-caste democratic polity inherit a common scriptural civilization without reproducing hereditary hierarchy: it settles, once and centrally, who may speak for the tradition once birth-based authority is publicly disqualified, and gives courts, movements, and communities a common standard for resolving religious-practice disputes.
% TRANSFER_FUNCTION: Moves interpretive authority and public legitimation from hereditary priestly lineages and orthodox monastic establishments to constitutional organs, the reformist scholarly establishment, and mobilized Dalit movements; moves control of temple administration and ritual precedence toward state-supervised egalitarian arrangements; and moves stigma onto lineage-based claims.
% ABSENT_VOICES: Village-level non-Brahmin ritual specialists and ordinary devotees whose lived religiosity fits neither Sanskritic orthodoxy nor constitutional reformism would object that the contest is conducted entirely over their heads; orthodox voices, where heard at all, appear as defendants in court rather than as participants in framing what conformity means. They are outside the courtroom-academic circuit where the settlement's meaning is actually produced.
% DISAPPEARANCE_RATIONALE: If the conformity requirement vanished overnight, temple governance would revert toward custodial control, personal-law adjudication would lose its constitutional anchor, Dalit political incorporation would lose its legal instruments, orthodox establishments would reclaim public interpretive jurisdiction, and the reformist scholarly apparatus would collapse for lack of a client — the entire settlement architecture of religion-and-state in the republic would reorganize within a decade.
% FOUNDING_PROBLEM: The arrangement was built to solve the contradiction between a newly founded constitutional democracy guaranteeing equality and a scriptural inheritance received for centuries as prescribing hereditary hierarchy: how can the republic honor both its founding charter and its civilizational texts without choosing openly between them?
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: orthodox establishments themselves confirm the conflict is live — their continuing litigation calendar is adversarial attestation that the reconciliation remains unresolved — and government atrocity statistics, census data on manual scavenging, and national human-rights reporting independently document that the hierarchy the settlement targets persists in practice. No party disputes that the founding problem is unfinished; they dispute only whose fault and whose solution it is.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45 at interval end): the settlement genuinely delivers a shared civic-interpretive baseline, but it also displaces a whole class of authorities, subjects religious life to state adjudication, and has accumulated an adjudicative-and-expertise complex whose growth outpaces some of its delivered outcomes. Suppression (0.58) is the cost of holding the settlement against persistent orthodox counter-mobilization — it is enforced, not self-sustaining — and the rising suppression_requirement series tracks the deliberate maturation of that enforcement machinery (temple-entry Acts, essential-practices jurisprudence, atrocity-law enforcement), which is why the story authors that series rather than leaving suppression static. Theater (0.30) rises across the interval: the founding decades were substantively transformative (concrete legal reform, mass conversion movements, real access gains), while later decades add a growing share of ceremonial constitutionalism — anniversary rhetoric, symbolic gestures, compliance performance — alongside continuing substance. Accessibility_collapse is low (0.35): the rival hereditary reading does not collapse when the reformist reading is understood; it persists robustly in social practice and political mobilization, which is precisely why ongoing enforcement is needed. Resistance is high (0.65): orthodox institutions litigate, mobilize politically, and non-comply in ritual practice. All three temporal series run on one shared seven-point grid (T = 0, 12, 25, 37, 50, 62, 75) so no metric is sampled against another's end-state; trajectories are monotonic rather than cyclical, driven by enforcement-capacity accumulation rather than oscillating crisis dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently, and the structural data is built to let them: from the orthodox payer seats the settlement is dispossession — an external power seizing the tradition's interpretive helm and stigmatizing their office; from the Dalit beneficiary seat it is the minimum condition of citizenship inside a civilization whose texts were weaponized against them; from the state seat it is a constitutional duty indistinguishable from the republic's own legitimacy. Same nominal polity, radically different experienced arrangements — the divergence falls out of the declared roles, power atoms, and exit options, not from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the three beneficiary seats toward the subsidized end: Dalit movements (organized, constrained exit — they gain instruments but cannot leave), the scholarly establishment (institutional, identity_locked — career and self-concept fused with the reading), and the state (dual-positioned agenda-setter/beneficiary — collects jurisdiction and legitimacy). Victim declarations drive the orthodox seats toward the target end, amplified by exit modulation: priestly lineages are identity_locked because the reading disqualifies the very identity that constitutes their authority — they cannot exit without ceasing to be what they are — so they sit nearer the full-target pole than their moderate power alone would suggest; mutt establishments are constrained but resourced, sitting slightly back from the pole; custom-observant communities bear real adjudication burdens yet receive partial civic benefits, placing them between. The excluded folk-specialist seat sits outside both frames and feeds no derivation — its absence is recorded, not scored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling an equality-guaranteeing constitutional order with a scriptural inheritance long received as prescribing hereditary hierarchy — remains live: atrocity statistics, persisting ritual exclusion, and the orthodox litigation calendar all attest it from outside the beneficiary set. Because the founding problem is live, no mandatrophy is declared and the zombie-flag consumer finds status=live aligned with verdict=world_rearranges. The tangled_rope claim is what prevents mislabeling in both directions: reading the settlement as pure coordination (rope) would erase the identifiable orthodox victims and the accumulating adjudicative rents; reading it as pure extraction (snare) would erase the genuine collective-action achievement — a plural polity inheriting a common scriptural civilization without reproducing birth hierarchy — and the broad net-benefit profile. The hybrid category holds both facts without letting either cover story absorb the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_contested_kernel,
    'This constraint is ONE reading (reformist_egalitarian_reading) of the contested kernel vedic_dharmic_corpus; what would adopting a sibling reading change structurally?',
    'Comparative structural analysis across the three instantiated readings: hereditary_monopoly_reading inverts the beneficiary/victim sets entirely (priestly lineages collect, Dalit communities pay); bhakti_devotional_reading relocates enforcement from state organs to devotional communities and dissolves the state-adjudication layer.',
    'If the hereditary sibling were institutionally ascendant, this story''s beneficiaries become its victims and vice versa; if the bhakti sibling were ascendant, the state''s agenda-setter seat empties and the constraint''s enforcement profile collapses toward voluntary compliance. Cross-reading comparison is only valid story-to-story, never averaged inside one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_contested_kernel, conceptual, 'Kernel-membership committer structure: one of three rival readings, each instantiating a different constraint.').

omega_variable(
    varna_essence_vs_accretion_philology,
    'Is the historical-accretion thesis actually supported by the best available philology and social history, or does the reformist reading impose anachronistic meaning on texts whose stratified receptions were earlier than the thesis allows?',
    'Textual-critical and historiographic scholarship on the Vedic, Dharmashastra, and commentarial layers, assessed independently of both orthodox custodians and movement scholarship.',
    'If accretion fails as a historical claim, this reading''s central empirically-contingent axiom loses its grounding, the reading''s legitimacy shifts wholly onto the conventional constitutional axiom, and its imposition costs on orthodox seats rise sharply; if accretion holds, part of the measured burden on orthodox institutions is the price of correcting a genuine misreading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_essence_vs_accretion_philology, empirical, 'Whether caste hierarchy is historical accretion or scriptural essence — the factual hinge of the reading.').

omega_variable(
    state_adjudication_rent_question,
    'Does transferring interpretive jurisdiction from priestly lineages to constitutional organs dissolve the old authority rents, or relocate them into a new adjudicative apparatus (court dockets, expert testimony, compliance industries) that collects its own returns?',
    'Track the growth and composition of the litigation-and-expertise complex attached to religious-practice adjudication relative to measurable outcomes (temple access realized, ritual exclusions removed); compare jurisdictions with weaker state adjudication of religious meaning.',
    'If adjudicative rents grow faster than delivered outcomes, the rising base_extractiveness series reflects a new collector layer rather than transition costs, pushing the effective profile toward heavier extraction; if outcomes track jurisdiction, the extraction is largely the operating cost of a functioning coordination achievement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_adjudication_rent_question, empirical, 'Whether state adjudication replaces priestly rents with its own.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of orthodox practice structural (legal compulsion, loss of institutional standing) or internalized (communities self-police conformity, treat constitutional conformity as settled even where enforcement lapses)?',
    'Post-deregulation trajectory: observe whether orthodox separateness and ritual exclusion revive in domains where enforcement capacity has visibly decayed; if suppression persists where the enforcing apparatus has withdrawn, a large internalized component is established.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — orthodox seats carry the conformity demand even where courts do not reach — and resistance figures understate latent opposition; if structural, enforcement decay would produce rapid behavioral reversion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized component of orthodox conformity.').

omega_variable(
    bhakti_sibling_resource_pressure,
    'How much of this reading''s institutional position rests on structural pressure it exerts on the bhakti_devotional_reading (state recognition, funding, temple-control precedents) rather than on independent persuasive force?',
    'Trace devotional-community autonomy under regimes of differing constitutional-intensity: where the reformist apparatus weakens, does bhakti-mediated authority expand to fill the interpretive vacuum without inheriting this reading''s victim structure?',
    'If the bhakti sibling thrives independently, this reading''s coordination claim narrows to its constitutional core; if bhakti communities depend on this reading''s legal umbrella, part of this reading''s beneficiary structure is derivative and its removal would cascade through the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_sibling_resource_pressure, conceptual, 'Downstream structural pressure this reading exerts on the devotional sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(vedi_tr_t12, observed).
narrative_ontology:measurement(vedi_tr_t25, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 25, 0.17).
narrative_ontology:measurement_basis(vedi_tr_t25, observed).
narrative_ontology:measurement(vedi_tr_t37, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 37, 0.21).
narrative_ontology:measurement_basis(vedi_tr_t37, observed).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement_basis(vedi_tr_t50, observed).
narrative_ontology:measurement(vedi_tr_t62, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 62, 0.27).
narrative_ontology:measurement_basis(vedi_tr_t62, observed).
narrative_ontology:measurement(vedi_tr_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(vedi_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement_basis(vedi_be_t12, observed).
narrative_ontology:measurement(vedi_be_t25, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 25, 0.36).
narrative_ontology:measurement_basis(vedi_be_t25, observed).
narrative_ontology:measurement(vedi_be_t37, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 37, 0.4).
narrative_ontology:measurement_basis(vedi_be_t37, observed).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement_basis(vedi_be_t50, observed).
narrative_ontology:measurement(vedi_be_t62, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 62, 0.44).
narrative_ontology:measurement_basis(vedi_be_t62, observed).
narrative_ontology:measurement(vedi_be_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement_basis(vedi_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(vedi_su_t12, observed).
narrative_ontology:measurement(vedi_su_t25, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(vedi_su_t25, observed).
narrative_ontology:measurement(vedi_su_t37, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 37, 0.53).
narrative_ontology:measurement_basis(vedi_su_t37, observed).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(vedi_su_t50, observed).
narrative_ontology:measurement(vedi_su_t62, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 62, 0.57).
narrative_ontology:measurement_basis(vedi_su_t62, observed).
narrative_ontology:measurement(vedi_su_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement_basis(vedi_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the dharmic tradition's authority structure' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. The hereditary_monopoly_reading is the upstream arrangement (long-established, high extraction, priestly-collected); this reformist reading is downstream of it — it defines itself against the hereditary arrangement and cites its abuses as justification — and exerts structural pressure on the bhakti_devotional_reading (state recognition, temple-control precedent, funding channels) without foreclosing it. Each member carries its own epsilon, beneficiary/victim structure, and claimed type; cross-reading comparison happens between files, never by averaging inside one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
