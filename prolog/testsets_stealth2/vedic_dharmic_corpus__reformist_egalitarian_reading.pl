% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Hermeneutic Regime over the Dharmic Corpus
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   The reformist egalitarian reading of the vedic_dharmic_corpus operates as
 *   an enforced constraint on textual meaning: constitutional equality
 *   principles govern what the corpus may be taken to require, caste
 *   hierarchy is officially classified as historical accretion rather than
 *   scriptural essence, and rational critique holds formal precedence over
 *   lineage authority. The settlement delivers a genuine coordination good —
 *   a single civic standard that lets a stratified civilization function as
 *   an equal-rights democracy — while extracting asymmetrically from the
 *   seats whose authority it retires: hereditary priests, monastic
 *   hierarchies, and caste councils lose standing, jurisdiction, and
 *   livelihood, and the adjudicating state accumulates interpretive
 *   jurisdiction with every ruling. Temporal grid: T=0 corresponds to
 *   approximately 1950 (Constitution in force, Article 17 abolishing
 *   untouchability); T=75 to approximately 2025. The claim/metric
 *   relationship is deliberate and unreconciled: the reading CLAIMS
 *   tangled_rope from the authoring seat, and the metrics independently
 *   describe moderately extractive, actively enforced, increasingly
 *   theatrical operation — divergence between computed per-seat verdicts and
 *   this claim is the datum the corpus exists to collect.
 *
 * KEY AGENTS:
 *   - - indian_state_judiciary: Agenda-setter and receipt seat (institutional/arbitrage) — adjudicates meaning, accumulates jurisdiction
 *   - - dalit_liberation_movements: Primary beneficiary (organized/identity_locked) — converts the reading into access and representation
 *   - - marginalized_caste_communities: Primary beneficiary with residual costs (powerless/trapped) — receives mediated access
 *   - - reformist_intellectuals: Secondary beneficiary (moderate/mobile) — supplies the scholarly apparatus
 *   - - hereditary_brahmin_priesthood: Primary target (moderate/identity_locked) — bears retirement of birth-authority
 *   - - orthodox_monastic_institutions: Secondary target (organized/constrained) — loses doctrinal jurisdiction piecemeal
 *   - - traditional_caste_councils: Local target (moderate/constrained) — sanctions criminalized
 *   - - vernacular_devotional_communities: Excluded voice (moderate/constrained) — no seat in the textual-rational contest
 *   - - constitutional_historians: Analytical observer (analytical/analytical) — maps the structure without holding a position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.58).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Hermeneutic Regime over the Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '88446dd6-029f-49a2-b88b-7f41fa3846c1').
narrative_ontology:cs_kernel_codification('88446dd6-029f-49a2-b88b-7f41fa3846c1', fixed_text).
narrative_ontology:cs_authority_grounding('88446dd6-029f-49a2-b88b-7f41fa3846c1', expertise).
narrative_ontology:cs_interpretation_layer_present('88446dd6-029f-49a2-b88b-7f41fa3846c1').
narrative_ontology:cs_reading_relation('88446dd6-029f-49a2-b88b-7f41fa3846c1', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('88446dd6-029f-49a2-b88b-7f41fa3846c1', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('88446dd6-029f-49a2-b88b-7f41fa3846c1', foundational, rational_critique_supersedes_traditional_authority).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_traditional_authority, holdable).
narrative_ontology:cs_axiom_grounding('88446dd6-029f-49a2-b88b-7f41fa3846c1', rational_critique_supersedes_traditional_authority, deontological).
narrative_ontology:cs_axiom('88446dd6-029f-49a2-b88b-7f41fa3846c1', foundational, caste_hierarchy_is_historical_accretion).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion, holdable).
narrative_ontology:cs_axiom_grounding('88446dd6-029f-49a2-b88b-7f41fa3846c1', caste_hierarchy_is_historical_accretion, empirically_contingent).
narrative_ontology:cs_axiom('88446dd6-029f-49a2-b88b-7f41fa3846c1', secondary, textual_meaning_conforms_to_constitutional_equality).
narrative_ontology:cs_axiom_status(textual_meaning_conforms_to_constitutional_equality, holdable).
narrative_ontology:cs_axiom_grounding('88446dd6-029f-49a2-b88b-7f41fa3846c1', textual_meaning_conforms_to_constitutional_equality, conventional).
narrative_ontology:cs_reference_frame('88446dd6-029f-49a2-b88b-7f41fa3846c1', egalitarian_core_historical_accretion).
narrative_ontology:cs_drift_state('88446dd6-029f-49a2-b88b-7f41fa3846c1', contemporary_post_ambedkarite_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('88446dd6-029f-49a2-b88b-7f41fa3846c1', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_liberation_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, marginalized_caste_communities).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_state_judiciary).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_intellectuals).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_brahmin_priesthood).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_monastic_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_caste_councils).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional courts and tribunals decide what the dharmic corpus may mean in public life: which practices are essential, which exclusions are void, who may administer temples, what counts as religion rather than superstition. Every ruling extends the bench's interpretive jurisdiction, and the institution accumulates doctrinal authority with each case it settles. It can revisit its own precedents, widening or narrowing doctrine as it chooses — it writes the rules it enforces.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_state_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_state_judiciary, beneficiary).

% Ambedkarite organizations, Navayana Buddhist sanghas, and social-justice parties built their program on the egalitarian reading: annihilation of caste, constitutional morality, conversion out of caste. Their political identity and moral vocabulary are constituted by this frame — abandoning it would dissolve the movement's self-understanding. They convert the reading into reservations, reserved representation, temple-entry victories, and dignity claims.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_liberation_movements, beneficiary,
    organized, generational, identity_locked, national).

% Communities formerly barred from temples, wells, schools, and priesthood gain legal access and quota-backed representation under the reformist settlement. The access arrives mediated by state certification: benefits require remaining officially legible as Scheduled Castes or Backward Classes, and enforcement of protection statutes frequently lags far behind the written guarantee. Individual exit from caste location is not available; the community's standing improves only as the settlement deepens.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, marginalized_caste_communities, beneficiary,
    powerless, biographical, trapped, national).

% Philologists, historians, rationalist activists, and constitutional lawyers supply the scholarly apparatus the egalitarian reading runs on: accretion histories, critical translations, rights litigation, textbook revision. Careers, university chairs, publishing niches, and activist funding depend on the frame's continued centrality. Their analytic skills transfer readily to adjacent fields if the frame fades, so their position is engaged but not captive.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_intellectuals, beneficiary,
    moderate, biographical, mobile, national).

% Lineage priests lose the legal standing to reserve ritual office and temple access by birth; reforms open priesthood eligibility and temple committees to outsiders, and courts repeatedly reject birth-based claims to office. Their authority rested on being the corpus's authorized readers — a claim the reformist frame explicitly retires. Leaving the vocation means abandoning lineage obligation, training, and social identity simultaneously; continuing means officiating inside arrangements that deny the premise of the office.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_brahmin_priesthood, payer,
    moderate, generational, identity_locked, national).

% Mathas, akharas, and peethas see their control over doctrine, succession, and institutional property circumscribed by court oversight and reform legislation. They retain substantial wealth, followings, and a parallel teaching authority, and they litigate energetically to defend autonomy — but each adverse ruling narrows the space in which their reading of the corpus governs public religious life.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_monastic_institutions, payer,
    organized, generational, constrained, national).

% Village and gotra councils that enforced marriage rules, purity codes, and social boycotts now operate under criminal statutes targeting caste atrocity and honour-based violence. Their sanctions still bite locally where state presence is thin, but their norm-making is formally illegitimate, prosecutable, and increasingly reported by the people it once disciplined.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_caste_councils, payer,
    moderate, immediate, constrained, regional).

% Bhakti-lineage devotees and non-textual practitioners center religious life on song, pilgrimage, festival, and guru devotion rather than scriptural argument. Both the orthodox textual defense and the reformist rational critique argue over documents they rarely read; their insistence that devotion, not hermeneutics, is the operative core of the tradition finds no seat in the courtroom or the seminar room.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, vernacular_devotional_communities, excluded,
    moderate, biographical, constrained, regional).

% Scholars of law, religion, and colonial history trace how the egalitarian reading formed — from Orientalist philology through Ambedkar's annihilation-of-caste address to the essential-religious-practices doctrine — and compare the Indian settlement with other states' management of scriptural tradition. They take no side in the dispute; they map its structure and its precedents.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_state_judiciary).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single civic-legal standard for what the dharmic corpus may mean in public life, so that a multi-caste democratic polity can operate without per-community negotiation of every access and status question — temple entry, ritual office, civil rights, and educational access are settled once, centrally, under constitutional supremacy rather than locality by locality.
% TRANSFER_FUNCTION: Moves interpretive authority and the status attached to it from hereditary priestly lineages, monastic hierarchies, and caste councils to constitutional courts, reformist scholarship, and previously excluded caste communities; moves concrete goods — ritual access, reserved offices, educational seats, elected positions — toward marginalized castes via the quota and protection machinery that rides on the reading.
% ABSENT_VOICES: Vernacular devotional communities and non-textual practitioners are structurally absent: the contest is staged as text-versus-text and reason-versus-tradition, and the devotional claim that lived practice outranks both hermeneutics and critique has no procedural seat. Orthodox laity who experience the reformist frame as external imposition are similarly spoken for by institutions they did not choose.
% DISAPPEARANCE_RATIONALE: If the reformist settlement vanished overnight, birth-based bars on temple entry and ritual office would regain legal enforceability, the reservation architecture would lose its interpretive foundation and collapse into ordinary patronage, Dalit political mobilization would lose its constitutional anchor and reorganize around raw numbers or exit, and the courts would surrender the religious-interpreter role they have held for seventy years — the public organization of the tradition would revert to whichever lineage could enforce its reading.
% FOUNDING_PROBLEM: Reconciling a scripturally stratified civilization with democratic citizenship: how can a polity founded on equal rights govern a society whose dominant religious corpus had been read for centuries as ordaining hierarchy, and how do the people that reading placed outside the pale obtain entry, office, and standing?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: National Crime Records Bureau atrocity statistics, UN Committee on the Elimination of Racial Discrimination concluding observations on caste, and empirical sociology of continued discrimination in marriage, labor, and schooling all attest the founding problem remains live. Orthodox institutions dispute the framing itself — they attest the problem was manufactured or solved long ago — which is itself signal that the genealogy is contested rather than self-served.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.45 is moderate by construction of the settlement: real transfers occur (ritual access, reserved offices, criminalized atrocity), but they ride on a coordination good with broad payoff, bounding extraction well below snare territory. Suppression 0.58 reflects statutory criminalization of caste practice and judicial invalidation of exclusionary claims — coercively real, yet short of total, since orthodox observance persists in private and institutional life continues under adverse law. Theater 0.32: the functional core (quotas, temple-entry enforcement, protection statutes) does measurable work, while a growing symbolic layer — commemorative constitutionalism, rhetorical commitments against persistently practiced discrimination — performs the settlement more than it executes it. Accessibility_collapse 0.48: once constitutional supremacy is accepted, hierarchical readings collapse in public and legal space but survive intact in private observance and institutional teaching, so alternatives half-persist. Resistance 0.6: seventy years of sustained counter-mobilization — litigation by mathas, mass orthodox politics, revivalist movements attacking the reading as colonial or foreign — marks a construct that must be actively defended, not a natural fact. All three metric series run on one shared seven-point grid so temporal analysis samples complete rows.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat classifications should diverge sharply. From the bench's seat the settlement is constitutional morality operating as designed — coordination it built and administers. From the lineage priest's seat the same structure is dispossession: a several-thousand-year office retired by unelected adjudicators, with identity_locked exit making the loss total rather than partial. From Dalit-community seats the settlement is liberation still incomplete — access granted on paper, delivered slowly, and conditioned on permanent state legibility. From the devotional outsider's seat the whole contest is a category error. The engine computes these divergences from power, exit, and directional data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality (subsidized): dalit_liberation_movements sit deepest at the beneficiary end, amplified by identity_locked exit — the movement cannot abandon the frame without dissolving. Marginalized_caste_communities are overridden upward from a near-zero derived d to 0.18: derivation from the beneficiary declaration alone would miss that the settlement requires them to remain officially legible as certified categories to receive anything, a residual cost the pure-beneficiary reading conceals; the override encodes benefit-with-mediation. The judiciary derives low d as declarer-beneficiary, which is correct for receipts but understates its agenda-setting control — captured instead in the gain_flow field. Declared victims derive high directionality: hereditary_brahmin_priesthood sit nearest the full-target end (identity_locked exit fuses person, vocation, and lineage into the lost authority), orthodox_monastic_institutions slightly less (constrained exit, retained resources), traditional_caste_councils targeted regionally where enforcement reaches. Scope is national for most seats, which modestly amplifies effective extraction through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Reading the settlement purely from the reformist seat yields rope — pure coordination, everyone net-benefiting — which erases the retired seats and the accumulating jurisdiction of the adjudicator. Reading it purely from the orthodox seat yields snare — pure usurpation — which erases the real transfers and the coordination good that made the transfers possible. Tangled rope forces both facts into one structure: coordination function (declared beneficiaries, genuine collective-action solution) AND asymmetric extraction (declared victims, active enforcement). On mandatrophy proper: the founding problem (caste exclusion) is corroborated as live by sources outside the benefiting parties, so no resolved-mandatrophy declaration is authored; the rising theater series is logged as the early symptom to watch — if the symbolic layer keeps growing while delivery stagnates, the settlement drifts toward piton dynamics with the mismatch flag firing on dead-status-plus-world-rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    varna_accretion_thesis_status,
    'Is the claim that caste hierarchy is historical accretion rather than scriptural essence textually and historically sustainable, or does the reformist reading survive only by overriding passages its own frame cannot absorb?',
    'Critical philology and reception history: manuscript transmission of the varna-provision passages, dharmashastra commentary lineages, and documented historical mutation of jati practice against the textual record.',
    'If the accretion thesis holds, the reformist regime is recovering the kernel''s core and its costs sit nearer coordination overhead; if it fails, the regime rests on state coercion of meaning and its effective extraction rises sharply toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_accretion_thesis_status, empirical, 'Textual sustainability of the historical-accretion account of caste.').

omega_variable(
    constitutional_supremacy_legitimacy,
    'Does requiring textual meaning to conform to constitutional equality principles constitute a legitimate term of civic membership, or a state usurpation of religious self-definition?',
    'Comparative constitutional analysis of how other polities bound scriptural authority, combined with assessment of whether the corpus itself contains resources (debate norms, dissenting lineages) that make the conformity requirement internal to the tradition rather than externally imposed.',
    'If usurpatory, the costs borne by orthodox seats are extraction rather than the price of membership and effective extraction exceeds the authored value; if internal, part of the orthodox burden is self-incurred through the tradition''s own prior exclusions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_supremacy_legitimacy, conceptual, 'Legitimacy status of constitutional supremacy over scriptural meaning.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the vedic_dharmic_corpus kernel (reading: reformist_egalitarian_reading); how would the beneficiary/victim structure and epsilon change under the sibling readings hereditary_monopoly_reading and bhakti_devotional_reading?',
    'Generate and compare the sibling stories: the hereditary reading authors high epsilon with Brahmin-lineage beneficiaries and outcaste victims; the bhakti reading authors a different victim set keyed to gatekeeping of devotional access rather than textual authority.',
    'Cross-reading comparison locates the kernel disagreement in the authority-source premise (birth versus devotion versus reason); classification verdicts that agree across all three readings describe the kernel, verdicts that diverge describe the readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame delta across sibling readings of the shared kernel.').

omega_variable(
    state_capture_drift,
    'Does the expanding judicial jurisdiction over religious meaning (essential-religious-practices doctrine, temple governance rulings) represent faithful execution of the egalitarian mandate or institutional self-aggrandizement by the adjudicating seat?',
    'Track whether rulings expand equality outcomes or expand the bench''s decision domain independent of equality outcomes; compare remedy design in cases where equality was achievable without extending jurisdiction.',
    'If self-aggrandizing, gain_flow concentrates further in the judiciary and the story drifts from tangled_rope toward snare; if faithful, the current receipt attribution is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_drift, empirical, 'Whether the enforcing seat is capturing the mandate it executes.').

omega_variable(
    reservation_regime_separability,
    'Is the hermeneutic constraint (egalitarian textual meaning) separable from the material quota regime (reservations, certified-category benefits) it travels with, or are they one bundled arrangement?',
    'Counterfactual policy analysis: jurisdictions or periods where egalitarian interpretation operated without quotas, or quotas without interpretive reform; measure whether each survives alone.',
    'If separable, this is two constraints with different epsilon values and the story should decompose into a hermeneutic story and a distributive story linked by network edges; if inseparable, the authored epsilon correctly prices the bundle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reservation_regime_separability, empirical, 'Separability of interpretive and redistributive components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(vedi_tr_t25, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(vedi_tr_t37, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 37, 0.24).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(vedi_tr_t62, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 62, 0.3).
narrative_ontology:measurement(vedi_tr_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 75, 0.32).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(vedi_be_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(vedi_be_t25, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(vedi_be_t37, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 37, 0.41).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(vedi_be_t62, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 62, 0.44).
narrative_ontology:measurement(vedi_be_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 75, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(vedi_su_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(vedi_su_t25, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(vedi_su_t37, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 37, 0.54).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(vedi_su_t62, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 62, 0.57).
narrative_ontology:measurement(vedi_su_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, resource_allocation).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the dharmic corpus requires' decomposes into three structurally distinct readings of one kernel (vedic_dharmic_corpus), each with its own epsilon, beneficiary set, and authority source. hereditary_monopoly_reading (birth-derived authority, divinely ordained varna) is the historically upstream claim — the reformist reading defines itself against it and cites it as the thing corrected; bhakti_devotional_reading (devotion over birth) shares the reformist conclusion while rejecting its rationalist ground. The reformist story links both siblings via affects_constraints; the family exists because 'the meaning of the corpus' measured through different authority sources yields different epsilon values, violating epsilon-invariance if forced into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, powerless, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
