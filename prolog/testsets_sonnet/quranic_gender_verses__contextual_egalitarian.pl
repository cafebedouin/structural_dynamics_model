% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Contextual-Egalitarian (Maqasid) Reading of Qur'anic Gender Verses
 *   domain: Islamic Jurisprudence / Legal Hermeneutics / Gender Studies
 *
 * SUMMARY:
 *   In contemporary Islamic legal reform, a growing body of scholarship and
 *   jurisprudence reads Qur'anic verses on inheritance, testimony, and
 *   marital guardianship not as timeless fixed ratios but as progressive
 *   interventions calibrated to 7th-century Arabian social conditions —
 *   interventions whose enduring authority lies in the equity principle
 *   (maqasid) they were advancing, not in the specific numbers or roles
 *   named. This reading has been operationalized in family-law reform in
 *   several jurisdictions and cited extensively by reformist fatwa councils
 *   and rights NGOs to argue for equal inheritance shares, equal testimonial
 *   weight, and reduced guardianship requirements. It coexists in contest
 *   with a literal-hierarchical reading (verses as direct permanent
 *   ordinance) and a progressive-abrogation reading (later verses textually
 *   supersede earlier ones). This story concerns only the
 *   contextual-egalitarian reading's own coordination and extraction
 *   structure.
 *
 * KEY AGENTS:
 *   - reformist_scholars: agenda_setter, organized/mobile — develop and promote the hermeneutic
 *   - womens_rights_ngos: beneficiary/agenda_setter, organized/mobile — fund and advocate for adoption
 *   - women_seeking_equal_inheritance_and_testimony: beneficiary, moderate/constrained — direct material beneficiaries where courts adopt the reading
 *   - traditional_qadis_losing_discretionary_authority: payer, institutional/constrained — lose interpretive discretion
 *   - patriarchal_family_elders: payer, moderate/trapped — lose informal control over dependents
 *   - traditionalist_ulama: excluded, organized/constrained — contest legitimacy but lack forum access
 *   - comparative_legal_historians: observer, analytical — document cross-reading effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.42).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.38).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.42).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Contextual-Egalitarian (Maqasid) Reading of Qur'anic Gender Verses").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "Islamic Jurisprudence / Legal Hermeneutics / Gender Studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c').
narrative_ontology:cs_kernel_codification('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', fixed_text).
narrative_ontology:cs_authority_grounding('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', expertise).
narrative_ontology:cs_interpretation_layer_present('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c').
narrative_ontology:cs_reading_relation('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', quranic_gender_verses__literal_hierarchical, influences).
narrative_ontology:cs_reading_relation('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', foundational, maqasid_principle_supersedes_particular_ruling).
narrative_ontology:cs_axiom_status(maqasid_principle_supersedes_particular_ruling, holdable).
narrative_ontology:cs_axiom_grounding('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', maqasid_principle_supersedes_particular_ruling, conventional).
narrative_ontology:cs_axiom('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', foundational, verse_specific_ratios_are_historically_indexed_not_eternal).
narrative_ontology:cs_axiom_status(verse_specific_ratios_are_historically_indexed_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', verse_specific_ratios_are_historically_indexed_not_eternal, empirically_contingent).
narrative_ontology:cs_reference_frame('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', classical_textual_literalism).
narrative_ontology:cs_drift_state('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2125e4ce-b1fb-47e3-b55b-42fc9ee5e04c', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_seeking_equal_inheritance_and_testimony).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_qadis_losing_discretionary_authority).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_family_elders).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, litigants_relying_on_settled_classical_rulings).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, maqasid_al_sharia_supremacy_doctrine).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, historical_contextualization_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and promote the maqasid-based hermeneutic in academic institutions, fatwa councils, and legal reform commissions. Their interpretive authority and career standing grow directly with adoption of this reading; they can move between jurisdictions and platforms if resisted locally, giving them real exit relative to the communities whose law they are reinterpreting.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    organized, generational, mobile, national).

% Fund litigation, drafting of model family-law codes, and public advocacy built on the contextual reading. They gain funding, legitimacy, and policy access as this reading is adopted into statute; they can redirect resources to other jurisdictions if a given reform effort stalls.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos, agenda_setter).

% Individual claimants in inheritance and testimony disputes who invoke the contextual reading to argue for equal shares or equal evidentiary weight. They gain a structural legal claim they previously lacked, but access to a sympathetic court, lawyer, or reform-minded qadi is uneven and depends on jurisdiction; many remain unable to actually invoke the reading despite its formal availability.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, women_seeking_equal_inheritance_and_testimony, beneficiary,
    moderate, biographical, constrained, local).

% Judges trained in and credentialed through classical fiqh whose rulings on inheritance, testimony, and guardianship are increasingly overturned or bypassed when appellate bodies adopt the maqasid framework. Their professional authority and the predictability of their docket erode; they cannot easily retrain into an entirely different interpretive tradition without reputational cost.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_qadis_losing_discretionary_authority, payer,
    institutional, biographical, constrained, national).

% Household and clan heads whose customary control over inheritance division and marriage arrangements depended on the literal reading being treated as settled divine law. When courts recognize the contextual reading, their informal leverage over dependents weakens; they have no venue to which they can relocate this authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_family_elders, payer,
    moderate, biographical, trapped, local).

% Parties, often male heirs or husbands, who structured expectations and prior agreements around the classical division of shares and testimony weight. A shift to the contextual reading can retroactively unsettle finalized or expected outcomes in ongoing disputes, and they have no forum for redress against the change in interpretive regime itself.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, litigants_relying_on_settled_classical_rulings, payer,
    powerless, biographical, trapped, local).

% Classical-school scholars who regard the historical-contextualization move itself as an illegitimate imposition of external liberal norms onto revelation. They are frequently excluded from reform commissions and international legal-development forums where the contextual reading is being operationalized, even though they represent large constituencies who would contest its legitimacy if given standing.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditionalist_ulama, excluded,
    organized, civilizational, constrained, global).

% Study how the contextual-egalitarian reading emerged, spread through fatwa councils and reform legislation, and interacts with the literal-hierarchical and progressive-abrogation readings. They document which populations gain and lose standing under each reading without themselves being a party to any of the three.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single interpretive framework — maqasid al-sharia read through 7th-century historical situatedness — that allows courts, legislatures, and scholars across diverse jurisdictions to update inheritance, testimony, and guardianship rulings without declaring the Qur'anic text itself in error, resolving the coordination problem of how to reform family law while retaining textual authority.
% TRANSFER_FUNCTION: Moves interpretive authority from classically trained qadis and patriarchal family structures to reformist scholars and rights-based organizations, and moves material entitlements (inheritance shares, evidentiary weight, guardianship discretion) from male heirs and family elders toward women litigants who can access courts applying this reading.
% ABSENT_VOICES: Traditionalist ulama and the constituencies who accept their authority are frequently not represented in the reform commissions, international legal-development bodies, and academic venues where the contextual reading is developed and promoted; they would contest both the hermeneutic method and its practical results but are structurally outside the rooms where the reading is operationalized into law.
% DISAPPEARANCE_RATIONALE: If courts and legislatures stopped applying the contextual-egalitarian reading, inheritance and testimony rulings would revert toward classical formulas in jurisdictions that had adopted reform statutes; women who had begun receiving equal shares or equal evidentiary weight under this reading would lose that claim; traditional qadis and family elders would regain the discretionary authority they had been losing; reformist scholars and NGOs would lose a primary vehicle for their institutional standing.
% FOUNDING_PROBLEM: Postcolonial and modern nation-states needed a way to reform family law to meet international human-rights commitments and domestic demands for gender equity without repudiating the Qur'an's authority or provoking a legitimacy crisis with religious constituencies — the contextual-egalitarian reading solves this by relocating the timeless normative content in maqasid rather than in the specific verses.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars and allied NGOs attest the founding problem (reconciling equity commitments with scriptural authority) remains live and the reading is a good-faith solution. Independent comparative legal historians and some secular human-rights researchers, outside the reformist camp, corroborate that formal legal equality remains substantially unrealized in practice even where the reading is adopted, suggesting the arrangement has partly become a legitimating discourse rather than a fully operative solution. Traditionalist ulama, also outside the benefiting parties, dispute that any 'founding problem' requiring reinterpretation exists at all, holding the classical reading was never in need of a maqasid override — their corroboration is a rejection of the premise itself, which is included here as the dissenting outside view.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).
:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end, rising from 0.18) because the reading transfers real interpretive authority and material entitlements from one set of parties (traditional qadis, family elders, litigants relying on classical rulings) to another (reformist scholars, NGOs, women claimants) — this is a genuine transfer, not merely coordination, but it also solves a real problem (reconciling scriptural authority with equity commitments) so the extraction is layered onto legitimate coordination rather than being pure rent-seeking. Suppression is moderate (0.38) and rising, reflecting that adoption of this reading increasingly requires active institutional enforcement — appellate override of qadi rulings, statutory codification, exclusion of traditionalist objections from reform venues — rather than voluntary convergence. Theater ratio stays comparatively low (0.22) because the underlying function (actual shifts in inheritance and testimony outcomes for at least some women) is substantially real, not merely performative, though it grows slowly as institutional adoption outpaces on-the-ground implementation in some jurisdictions. Resistance is high (0.72) reflecting sustained, organized pushback from traditionalist ulama and constituencies who regard the hermeneutic itself as illegitimate. Accessibility collapse is moderate-low (0.35): the literal-hierarchical and progressive-abrogation readings remain fully available and actively practiced alternatives — this reading has not foreclosed them, it competes with them.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and womens_rights_ngos sit near the beneficiary end: they gain interpretive authority, funding, and institutional standing as the reading is adopted, and both have mobile exit options (can relocate advocacy to more receptive jurisdictions). Women seeking equal inheritance and testimony are also beneficiaries in structural terms, but their exit options are constrained by jurisdiction and access to sympathetic courts — the benefit is real but unevenly realized. Traditional qadis and patriarchal family elders sit near the target end: they lose discretionary or informal authority through the same mechanism that produces the reformist gain, and neither has meaningful exit (qadis are institutionally embedded; elders have no alternative venue for the authority they are losing). Litigants relying on settled classical rulings are trapped payers: they had no forum to contest the change in interpretive regime itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling equity commitments with scriptural authority in modern family law) remains genuinely live in most jurisdictions applying this reading — it has not become a dead mandate maintained only by institutional inertia. However, the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals a partial capture risk worth tracking: independent legal historians note that formal adoption of the reading sometimes outpaces substantive implementation, meaning the reading can function partly as legitimating discourse (satisfying international human-rights optics) without full operative effect — this is the theater_ratio's slow upward drift, not yet dominant but worth monitoring under T17.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the contextual-egalitarian reading a recovery of the Qur''an''s own internal logic (maqasid as textually intended), or a modern normative commitment (liberal gender equity) read back into the text under cover of historical method?',
    'This is not resolvable by further textual analysis alone — it depends on prior commitments about whether maqasid al-sharia is itself a classically attested interpretive category (traditionalists dispute its scope) or a modern hermeneutic innovation. Comparative analysis of pre-modern maqasid literature versus its contemporary invocation could partially inform, but the underlying disagreement is a live theological and methodological dispute, not an empirical one with a determinable answer.',
    'If judged a recovery of authentic classical method, the reading''s legitimacy claim strengthens against literal_hierarchical; if judged a modern importation, it weakens relative to both siblings and its extraction from traditional qadis'' authority looks more like displacement than correction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether maqasid-based reinterpretation recovers or invents the equity principle it claims to find in the text.').

omega_variable(
    sibling_reading_delta,
    'How would the beneficiary/victim structure and extraction level differ under the literal_hierarchical or progressive_abrogation siblings for the same underlying verses?',
    'Compare this story''s authored ε (0.42, moderate, rising) and stakeholder set against the separately authored literal_hierarchical story (expected: women in victim set, patriarchal elites and traditional courts as beneficiaries, likely lower reformist-driven extraction but higher structural suppression of women''s claims) and progressive_abrogation story (expected: intermediate structure, contested naskh legitimacy, different victim set around scholars who reject abrogation of legal verses).',
    'Confirms these are three genuinely distinct constraints under the ε-invariance principle rather than one constraint viewed three ways — each has its own ε, beneficiaries, and victims, linked via network.affects_constraints rather than merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Documents the expected structural delta between this reading and its two siblings in the same kernel.').

omega_variable(
    implementation_gap_ambiguity,
    'Is the rising theater_ratio (0.10 to 0.22) evidence that the reading''s formal legal adoption is beginning to outpace substantive enforcement (a legitimating-discourse drift), or simply the ordinary lag between statutory change and social outcome that any genuine reform exhibits?',
    'Track the ratio of jurisdictions with contextual-reading statutes to actual case outcomes reflecting equal shares/testimony over the next measurement interval; a widening gap over time would support the drift reading, a stable or narrowing gap would support ordinary implementation lag.',
    'If drift is confirmed, this reading edges toward piton-adjacent dynamics (formal adoption as institutional performance) even while retaining genuine coordination function; if lag is confirmed, the tangled_rope classification remains stable as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_gap_ambiguity, empirical, 'Whether rising theater ratio signals legitimating drift or ordinary reform-implementation lag.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__contextual_egalitarian, theater_ratio, 8, 0.13).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__contextual_egalitarian, theater_ratio, 16, 0.16).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__contextual_egalitarian, theater_ratio, 24, 0.19).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__contextual_egalitarian, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__contextual_egalitarian, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quranic_gender_verses kernel (contextual_egalitarian, literal_hierarchical, progressive_abrogation), each authored as an independent ε-invariant constraint per the ε-invariance principle. They are linked bidirectionally via affects_constraints because adoption or delegitimation of any one reading structurally shifts the resource availability and legitimacy conditions of the other two — a jurisdiction's move toward this reading directly displaces the operative authority of the literal_hierarchical reading in that jurisdiction's courts, and creates competitive pressure against the naskh-based progressive_abrogation reading as an alternative reform vehicle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
