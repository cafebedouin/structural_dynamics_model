% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate Unmediated Sovereignty (Loyalist Reading)
 *   domain: political_philosophy/constitutional_systems
 *
 * SUMMARY:
 *   The imperial mandate of heaven in East Asian political theory centers on
 *   legitimacy to rule. This constraint instantiates ONE READING of that
 *   contested kernel: the loyalist restoration reading, which holds that
 *   divine mandate requires the emperor to exercise sovereignty directly and
 *   unmediated by intermediary institutions. Under this reading, the
 *   shogunate's centuries-long exercise of governance represents
 *   institutional usurpation masked as legitimate delegation — a reading that
 *   delegitimizes feudal structures and requires institutional rupture (the
 *   Meiji Restoration) to restore constitutional alignment. The competing
 *   reading (bakufu_delegation_reading) holds that delegation is consistent
 *   with the mandate tradition and that shogunal governance lawfully
 *   exercises delegated imperial authority. These are not disagreements over
 *   facts about what happened historically; they are disagreements over how
 *   to READ the mandate's constitutional meaning. This story instantiates
 *   only the loyalist reading as a structurally coherent constraint — not the
 *   bakufu reading, not a split-the-difference position, but the clean
 *   structural implications of loyalism alone.
 *
 * KEY AGENTS:
 *   - imperial_court: The agenda-setter. Claims legitimacy from unbroken descent; under loyalism, must exercise administrative and military governance directly to remain legitimate.
 *   - loyalist_bureaucracy: Primary beneficiary. Scholars, administrators, and strategists whose career and ideology depend on restoration of unmediated imperial sovereignty. Identity is fused to the ideological project.
 *   - bakufu_shogunate: Primary institutional payer. Delegitimized as usurpation under this reading; institutional rupture is required for constraint satisfaction.
 *   - samurai_class: Secondary organizational payer. Identity locked to feudal service structures (bushido, service to daimyo). Restoration requires reorienting loyalty directly to throne, dissolving the feudal relationships that constitute samurai identity.
 *   - regional_daimyo: Tertiary powerful payer. Authority derives from shogunal delegation; restoration cascades into loss of autonomy.
 *   - foreign_powers: Excluded. Would negotiate with shogunate; restoration requires they engage only with direct imperial representatives.
 *   - modernization_reformers: Dual position. Benefit from delegitimization of shogunal conservatism as obstacle to military and technological modernization. Yet constrained by requirement that reforms be explicitly imperial, not merchant-led or bottom-up.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.72).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate Unmediated Sovereignty (Loyalist Reading)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/constitutional_systems").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '83672b55-6cce-40a8-9864-990bc006447b').
narrative_ontology:cs_kernel_codification('83672b55-6cce-40a8-9864-990bc006447b', fixed_text).
narrative_ontology:cs_authority_grounding('83672b55-6cce-40a8-9864-990bc006447b', lineage).
narrative_ontology:cs_interpretation_layer_present('83672b55-6cce-40a8-9864-990bc006447b').
narrative_ontology:cs_reading_relation('83672b55-6cce-40a8-9864-990bc006447b', imperial_mandate__bakufu_delegation_reading, coexists_with).
narrative_ontology:cs_axiom('83672b55-6cce-40a8-9864-990bc006447b', foundational, mandate_requires_unmediated_sovereignty).
narrative_ontology:cs_axiom_status(mandate_requires_unmediated_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('83672b55-6cce-40a8-9864-990bc006447b', mandate_requires_unmediated_sovereignty, deontological).
narrative_ontology:cs_axiom('83672b55-6cce-40a8-9864-990bc006447b', foundational, delegated_governance_as_institutional_usurpation).
narrative_ontology:cs_axiom_status(delegated_governance_as_institutional_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('83672b55-6cce-40a8-9864-990bc006447b', delegated_governance_as_institutional_usurpation, deontological).
narrative_ontology:cs_reference_frame('83672b55-6cce-40a8-9864-990bc006447b', unmediated_imperial_sovereignty).
narrative_ontology:cs_drift_state('83672b55-6cce-40a8-9864-990bc006447b', contemporary_bakufu_dominance, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('83672b55-6cce-40a8-9864-990bc006447b', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_bureaucracy).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, bakufu_shogunate).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, regional_daimyo).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, modernization_reformers).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, modernization_reformers).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, unified_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, imperial_administrative_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims legitimacy from unbroken descent and mandate to govern directly. Under this reading, the court must actively exercise administrative and military sovereignty to remain legitimate. Delegates to the shogunate represent usurpation of mandate; restoration requires resuming direct control of government, military, and foreign relations. The court articulates the constraint's meaning and enforces it through loyalist movements and institutional reconstruction.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Career administrators and scholars whose position, prestige, and ideology depend on the empire's legitimacy doctrine and the vision of restored imperial administrative supremacy. Restoration of unmediated governance creates positions, authority, and historical vindication for the loyalist faction. Identity is fused to the restoration narrative; departure means ideological betrayal.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_bureaucracy, beneficiary,
    organized, biographical, identity_locked, national).

% Under this reading, the shogunate's legitimacy is delegitimized as usurpation — it governs only at imperial sufferance, not through independent mandate. This reading imposes institutional rupture as the price of constraint compliance: the shogunate cannot reconcile active governance with the reading's requirement of unmediated imperial sovereignty. Institutional reform or abolition becomes mandatory for constraint satisfaction.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, bakufu_shogunate, payer,
    institutional, generational, trapped, national).

% Samurai identity and code (bushido) are historically tied to service to daimyo and shogun. Under the loyalist reading, this institutional relationship is reframed as usurpation requiring termination. Samurai who refuse to redirect loyalty directly to the throne face marginalization; those who comply face dissolution of the feudal relationships that constitute their identity and social position.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class, payer,
    organized, biographical, identity_locked, national).

% Regional lords whose authority derives from shogunal delegation and feudal contracts. The loyalist reading delegitimizes shogunal authority, which cascades: daimyo authority becomes contingent on direct imperial legitimation. Restoration of unmediated sovereignty requires subordination to imperial authority or institutional dissolution. Their regional autonomy is the extractive target of this constraint.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, regional_daimyo, payer,
    powerful, biographical, constrained, national).

% Would engage with Japan's shogunate on equal institutional footing. Under the loyalist reading, foreign relations require explicit imperial initiative and direct imperial authorization. The shogunate's ability to negotiate independently is structurally denied. Foreign powers are excluded from the internal dispute but trapped by the constraint's requirement that they engage only with the emperor's direct representatives.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, excluded,
    institutional, biographical, trapped, global).

% Advocates for adopting Western military, administrative, and technological models to compete with colonial powers. They benefit from the constraint's delegitimization of shogunal conservatism and feudal structures as obstacles to modernization. Yet they also bear costs: the constraint requires that modernization initiatives be explicitly imperial, limiting space for bottom-up reform or merchant-led development outside direct imperial oversight.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, modernization_reformers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, modernization_reformers, payer).

% Examine whether the mandate doctrine is internally coherent across its readings, whether the loyalist interpretation is historically defensible, and what structural consequences follow from adopting one reading over another. They take testimony from all parties, reconstruct textual and institutional histories, and analyze the mandate's evolution under different framings.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, constitutional_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies sovereignty and legitimacy to prevent competing claims to authority from fragmenting the realm and paralyzing foreign relations and military defense. The constraint solves the problem of who legitimately commands armies, sets foreign policy, and initiates institutional change — by requiring that authority flow directly from the throne rather than through intermediary governance structures.
% TRANSFER_FUNCTION: Transfers administrative authority, military command, and foreign-policy decision-making from the shogunate and regional daimyo to the imperial court and loyalist bureaucracy. Redirects samurai military loyalty from feudal lords to imperial command. Removes the shogunate's capacity for independent governance.
% ABSENT_VOICES: The shogunate cannot speak legitimately from within this reading because its institutional role is reframed as usurpation. Merchants and local administrators who depended on shogunal stability for economic continuity have no institutionalized voice in the restoration debate. Foreign powers are systematically excluded from negotiating the internal constitutional rearrangement — they are locked into engaging only with imperial representatives after restoration.
% DISAPPEARANCE_RATIONALE: If the constraint requiring unmediated imperial sovereignty vanished, the bakufu reading's interpretation would prevail; shogunal institutional legitimacy would be restored; feudal governance structures would remain intact; samurai would maintain identity through service to daimyo; regional autonomy would be preserved. The entire institutional architecture would reorganize around mediated sovereignty and shogunal delegation rather than direct imperial control. The administrative continuity that persisted under Edo shogunal governance would replace the institutional rupture imposed by restoration.
% FOUNDING_PROBLEM: The mandate to govern from the throne is presented as the eternal constitutional principle grounding imperial legitimacy. The founding problem is institutional decay over centuries: the shogunate gradually usurped direct governance and concealed this under a fiction of delegation, creating a gap between the mandate's true requirement (unmediated imperial sovereignty) and actual institutional practice (delegated shogunal governance). The gap is presented as not merely a historical accident but a structural usurpation requiring correction.
% FOUNDING_PROBLEM_CORROBORATION: Loyalist scholars, imperial court historians, and Meiji-period reformers attest the founding problem is real: the shogunate's governance represents institutional usurpation concealed as delegation, and restoration is restoration of constitutional alignment. Bakufu-aligned historians and Edo scholars contest this fundamentally, arguing that shogunal delegation is consistent with the mandate tradition and that the constraint is a novel reading, not a recovered necessity. Comparative constitutional historians note the founding problem's reality is itself the site of the reading contest — both readings appeal to classical sources; the sources are genuinely ambiguous enough to support both interpretations. No neutral party external to the dispute attests the founding problem as a discovered fact; its reality is constitutively tied to which reading is accepted.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 at interval end because this reading systematically transfers administrative authority, military command, and foreign-policy initiative from the shogunate and regional daimyo to the imperial court and loyalist bureaucracy. The reading is not claiming this is unjust extraction — it is claiming this is the restoration of legitimate constitutional structure. But measured structurally, the constraint operates extractively: it denies institutional autonomy to the shogunate, samurai, and daimyo while enriching and vindicating the imperial court and loyalist faction. Suppression reaches 0.72 because constraint satisfaction requires active enforcement against shogunal resistance, samurai identity-resistance, and regional autonomy claims — the constraint cannot hold without sustained institutional pressure. Theater ratio rises from 0.15 to 0.41 over the interval because early in the Edo period the loyalist reading is marginal intellectual doctrine with little institutional force; by the late Edo period (c.1780–1830) it becomes the rallying narrative for active movements (scholars, samurai reformers, regional lords) whose institutional campaigns increasingly dominate court discussion and policy, yet the shogunate remains in formal control — theatrical maintenance of shogunal authority alongside accelerating loyalist mobilization. The grid shows how suppression and accessibility collapse intensify at the organizational level (daimyo, bakufu structures) relative to individual level: the constraint operates most directly on institutional actors whose autonomy is at stake, less directly on individuals whose roles can be reframed within continuity. Resistance is concentrated at the organizational level (shogunate, major daimyo blocs) and is highest there (0.71) because institutional survival is directly threatened. Class-level resistance (peasants, merchants, samurai as a class) is lower because the constraint does not directly target livelihoods in the same way it targets institutional authority.
 *
 * PERSPECTIVAL GAP:
 *   The imperial court seat and the loyalist bureaucracy seat should perceive this as legitimate restoration, not extraction — they see shogunal governance as the extraction (institutional usurpation of imperial prerogative), and loyalism as recovery. The shogunate, samurai, and daimyo seats perceive the loyalty requirement as delegitimization and institutional rupture. The engine computes per-seat classifications from this structural asymmetry: seats benefiting from imperial vindication should compute differently (more coordination-favorable) than seats whose institutional existence is delegitimized. The authored claim is tangled_rope because genuine coordination function exists (solving the problem of unified soverenity) alongside asymmetric extraction (transfer of authority from shogunate to court). The metrics reflect the extractive asymmetry being active and enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial court: d near 0.0 (full beneficiary — collects institutional authority, vindication, governing power). Loyalist bureaucracy: d near 0.2 (beneficiary — careers and ideology aligned with restoration narrative; exit is identity_locked; they exit only through ideological betrayal). Bakufu shogunate: d near 0.95 (full target — institutional authority is the extraction object; exit is trapped because shogunal identity IS the institutional structure). Samurai class: d near 0.85 (high target — feudal identity is the extraction object; exit is identity_locked; samurai cannot exit loyalty restructuring without dissolving the identity). Regional daimyo: d near 0.75 (target — regional autonomy is constrained; exit is constrained because daimyo authority derives from shogunal delegation, which this reading delegitimizes). Foreign powers: excluded, not targets or beneficiaries — they are trapped by the constraint's exclusion of shogunal negotiating authority. Modernization reformers: d near 0.45 (mixed — they benefit from delegitimization of shogunal conservatism, but are constrained by requirement that reforms be explicitly imperial).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims tangled_rope because it combines genuine coordination (unifying sovereignty to prevent institutional fragmentation and resolve competing claims to legitimacy) with asymmetric extraction (transferring authority from shogunate to court). The founding problem (institutional decay / institutional usurpation by the shogunate) is alive but contested — loyalist scholars argue the decay is real and ongoing; bakufu-aligned scholars argue shogunal governance is legitimate delegation, not usurpation. The disappearance_verdict is world_rearranges: if this constraint vanished, the shogunate's legitimacy would be restored, institutional continuity preserved, and the entire constitutional architecture reorganized around mediated governance. This indicates the constraint is not a natural law or irreducible feature of governance — it is a constructed constitutional claim requiring active institutional work to maintain. The theater_ratio rise over the interval (0.15 → 0.41) indicates growing performative maintenance: late Edo court rituals increasingly invoke restoration themes, scholarly commentary increasingly frames shogunal governance as provisional and corrupt, loyalist movements claim institutional moral authority even as shogunal administrative apparatus persists. The gap between loyalist rhetorical dominance and actual institutional control creates theatrical maintenance dynamics. Yet the constraint is not a piton — it has genuine beneficiaries (imperial court, loyalist bureaucracy) who actively defend it and who gain real institutional power from restoration. A piton would require no concentrated beneficiary; here the court and loyalist faction are clearly enriched by constraint persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_contingency,
    'Is the loyalist reading''s requirement of unmediated sovereignty a necessary logical consequence of the mandate doctrine, or a reading-dependent interpretation of ambiguous foundational texts?',
    'Textual reconstruction of classical sources (Confucian texts, Japanese imperial edicts, shogunal proclamations) by scholars trained in multiple readings; comparative analysis of how other East Asian kingdoms interpreted delegation versus unmediated sovereignty; analysis of which interpretive lineages influenced each reading.',
    'If unmediated sovereignty is logically entailed, the bakufu reading is incoherent and the loyalist constraint is structurally necessary. If the texts are ambiguous, both readings are defensible, and the constraint is a chosen interpretation, not a discovered necessity. This bears directly on whether the constraint should classify as mountain (natural law) or constructed (tangled_rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_interpretation_contingency, conceptual, 'Whether unmediated sovereignty requirement is logically entailed or reading-dependent').

omega_variable(
    institutional_authority_vs_legitimacy_separation,
    'Are delegated administrative authority and mandate-granting legitimacy structurally inseparable or separable?',
    'Historical analysis of whether shogunal appeals to legitimacy were ever accepted as valid by foreign powers, vassal states, and competing Japanese factions; analysis of how the court used its legitimacy-granting capacity even while the shogunate governed; investigation of whether the court ever withdrew legitimacy from the shogunate or whether that threat was structural but unrealized.',
    'If authority and legitimacy are separable, the bakufu reading is consistent with the mandate, and the loyalist reading is claiming institutional reorganization, not constitutional restoration. If inseparable, the loyalist reading''s requirement of unmediated sovereignty is more strongly anchored. This bears on whether the constraint''s extraction is exploitation of institutional ambiguity (snare dynamics) or genuine coordination difference (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_legitimacy_separation, empirical, 'Whether mandate legitimacy and administrative authority are structurally separable').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (legal barriers, military enforcement, institutional exclusion) or internalized (samurai and daimyo accept the loyalist framing as legitimate)?',
    'Analysis of resistor narratives: did samurai who opposed restoration do so because they saw it as institutional injustice (internalizing the loyalist claim and rejecting it) or because they opposed change to their position regardless of framing? Post-restoration behavior: did samurai and daimyo accept the new hierarchies as legitimate within the restored constitutional framework, or did they harbor resentment of the legitimacy shift?',
    'If internalized, the constraint''s suppression persists beyond institutional enforcement because targets have accepted the reading''s legitimacy framing. If structural, suppression drops sharply if enforcement resources relax. This affects whether post-restoration stability indicates the constraint solved a real coordination problem (loyalist claim) or simply succeeded through force (snare dynamics).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of shogunal/samurai/daimyo resistance is structural or internalized').

omega_variable(
    reading_kernel_contingency,
    'Is the loyalist reading''s instantiation of the imperial mandate contingent on late-Edo intellectual ferment and foreign pressure, or is it the necessary logical explication of pre-existing mandate doctrine?',
    'Timeline analysis: did loyalist interpretation arise as a novel synthesis in response to Meiji-period circumstances, or does it appear explicitly in classical sources? Genealogy of the reading: which scholars first articulated unmediated sovereignty as the mandate''s requirement, and what were their motivations and audiences? Comparison with earlier periods'' interpretations.',
    'If the reading is novel, it is a constructed reading chosen for its political utility in the context of foreign pressure and institutional crisis — the constraint is a reading of ambiguous texts under specific historical conditions, not a discovered necessity. If the reading is classical, it has longer lineage, though still potentially contestable. This affects confidence in whether the constraint represents true constitutional difference or strategic reinterpretation for political purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contingency, conceptual, 'Whether loyalist reading is classical or novel/contingent on Meiji-period conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1600, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1600, 0.15).
narrative_ontology:measurement_basis(impe_tr_t1600, observed).
narrative_ontology:measurement(impe_tr_t1700, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement_basis(impe_tr_t1700, observed).
narrative_ontology:measurement(impe_tr_t1780, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1780, 0.32).
narrative_ontology:measurement_basis(impe_tr_t1780, observed).
narrative_ontology:measurement(impe_tr_t1830, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1830, 0.41).
narrative_ontology:measurement_basis(impe_tr_t1830, observed).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.41).
narrative_ontology:measurement_basis(impe_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t1600, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement_basis(impe_be_t1600, observed).
narrative_ontology:measurement(impe_be_t1700, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement_basis(impe_be_t1700, observed).
narrative_ontology:measurement(impe_be_t1780, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1780, 0.61).
narrative_ontology:measurement_basis(impe_be_t1780, observed).
narrative_ontology:measurement(impe_be_t1830, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1830, 0.68).
narrative_ontology:measurement_basis(impe_be_t1830, observed).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.68).
narrative_ontology:measurement_basis(impe_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1600, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1600, 0.48).
narrative_ontology:measurement_basis(impe_su_t1600, observed).
narrative_ontology:measurement(impe_su_t1700, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1700, 0.52).
narrative_ontology:measurement_basis(impe_su_t1700, observed).
narrative_ontology:measurement(impe_su_t1780, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1780, 0.68).
narrative_ontology:measurement_basis(impe_su_t1780, observed).
narrative_ontology:measurement(impe_su_t1830, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1830, 0.72).
narrative_ontology:measurement_basis(impe_su_t1830, observed).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.72).
narrative_ontology:measurement_basis(impe_su_t1868, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1600, tn=1868
narrative_ontology:measurement(impe_grid_01, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(class), 1600, 0.48).
narrative_ontology:measurement(impe_grid_02, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(class), 1868, 0.76).
narrative_ontology:measurement(impe_grid_03, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(individual), 1600, 0.55).
narrative_ontology:measurement(impe_grid_04, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(individual), 1868, 0.82).
narrative_ontology:measurement(impe_grid_05, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(organizational), 1600, 0.62).
narrative_ontology:measurement(impe_grid_06, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(organizational), 1868, 0.88).
narrative_ontology:measurement(impe_grid_07, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(structural), 1600, 0.72).
narrative_ontology:measurement(impe_grid_08, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(structural), 1868, 0.85).
narrative_ontology:measurement(impe_grid_09, imperial_mandate__loyalist_restoration_reading, resistance(class), 1600, 0.38).
narrative_ontology:measurement(impe_grid_10, imperial_mandate__loyalist_restoration_reading, resistance(class), 1868, 0.62).
narrative_ontology:measurement(impe_grid_11, imperial_mandate__loyalist_restoration_reading, resistance(individual), 1600, 0.42).
narrative_ontology:measurement(impe_grid_12, imperial_mandate__loyalist_restoration_reading, resistance(individual), 1868, 0.58).
narrative_ontology:measurement(impe_grid_13, imperial_mandate__loyalist_restoration_reading, resistance(organizational), 1600, 0.68).
narrative_ontology:measurement(impe_grid_14, imperial_mandate__loyalist_restoration_reading, resistance(organizational), 1868, 0.71).
narrative_ontology:measurement(impe_grid_15, imperial_mandate__loyalist_restoration_reading, resistance(structural), 1600, 0.55).
narrative_ontology:measurement(impe_grid_16, imperial_mandate__loyalist_restoration_reading, resistance(structural), 1868, 0.64).
narrative_ontology:measurement(impe_grid_17, imperial_mandate__loyalist_restoration_reading, stakes_inflation(class), 1600, 0.42).
narrative_ontology:measurement(impe_grid_18, imperial_mandate__loyalist_restoration_reading, stakes_inflation(class), 1868, 0.68).
narrative_ontology:measurement(impe_grid_19, imperial_mandate__loyalist_restoration_reading, stakes_inflation(individual), 1600, 0.38).
narrative_ontology:measurement(impe_grid_20, imperial_mandate__loyalist_restoration_reading, stakes_inflation(individual), 1868, 0.71).
narrative_ontology:measurement(impe_grid_21, imperial_mandate__loyalist_restoration_reading, stakes_inflation(organizational), 1600, 0.58).
narrative_ontology:measurement(impe_grid_22, imperial_mandate__loyalist_restoration_reading, stakes_inflation(organizational), 1868, 0.81).
narrative_ontology:measurement(impe_grid_23, imperial_mandate__loyalist_restoration_reading, stakes_inflation(structural), 1600, 0.65).
narrative_ontology:measurement(impe_grid_24, imperial_mandate__loyalist_restoration_reading, stakes_inflation(structural), 1868, 0.79).
narrative_ontology:measurement(impe_grid_25, imperial_mandate__loyalist_restoration_reading, suppression(class), 1600, 0.38).
narrative_ontology:measurement(impe_grid_26, imperial_mandate__loyalist_restoration_reading, suppression(class), 1868, 0.68).
narrative_ontology:measurement(impe_grid_27, imperial_mandate__loyalist_restoration_reading, suppression(individual), 1600, 0.35).
narrative_ontology:measurement(impe_grid_28, imperial_mandate__loyalist_restoration_reading, suppression(individual), 1868, 0.62).
narrative_ontology:measurement(impe_grid_29, imperial_mandate__loyalist_restoration_reading, suppression(organizational), 1600, 0.55).
narrative_ontology:measurement(impe_grid_30, imperial_mandate__loyalist_restoration_reading, suppression(organizational), 1868, 0.81).
narrative_ontology:measurement(impe_grid_31, imperial_mandate__loyalist_restoration_reading, suppression(structural), 1600, 0.58).
narrative_ontology:measurement(impe_grid_32, imperial_mandate__loyalist_restoration_reading, suppression(structural), 1868, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imperial_mandate__loyalist_restoration_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% The imperial mandate constraint family consists of two structurally distinct constraints, each instantiating a different reading of the same kernel: (1) loyalist_restoration_reading (this story) — divine mandate requires unmediated imperial sovereignty; (2) bakufu_delegation_reading — divine mandate operates through institutional delegation and remains consistent with shogunal governance. These are not competing measurements of a single constraint; they are different constraints extracted from the same contested kernel. The loyalist reading has higher measured extractiveness (0.68) and suppression (0.72) because unmediated sovereignty systematically transfers authority from shogunate to court. The bakufu reading has lower extractiveness and suppression because delegation is presented as legitimate governance framework. Both readings exist as live positions in the historical record; neither logically forecloses the other within a single coherent framework — they coexist as different parties' interpretations. The engine should analyze whether the loyalist constraint's extraction profile is consistent with its coordination claim (genuine unification of sovereignty) or whether the constraint's primary function is institutional capture by the imperial faction (snare dynamics). The network edge indicates that invalidating or resolving one reading constrains the logical space for the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__loyalist_restoration_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
