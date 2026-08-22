% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections: Guarantor Reading (International Supervision)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Treaty of Lausanne (1923) includes protections for religious
 *   minorities in Turkey — Christian Orthodox, Armenian Apostolic, and Jewish
 *   communities. The guarantor reading frames these protections as
 *   international obligations enforceable through external mechanisms:
 *   guarantor state diplomacy and European human rights courts (ECtHR). This
 *   reading contests Turkey's domestic restrictive interpretation, which
 *   limits Lausanne protections to individual worship rights while treating
 *   institutional property, theological education, and administrative
 *   autonomy as matters of Turkish domestic law. The guarantor reading argues
 *   that the treaty's text and intent require external supervision of
 *   minority protections, not Turkish unilateral interpretation. This is ONE
 *   READING of the contested Lausanne kernel; it differs structurally from
 *   the expansive reading (which grounds institutional claims in Ottoman
 *   continuity) and the restrictive reading (which grounds them in Turkish
 *   sovereignty alone).
 *
 * KEY AGENTS:
 *   - Turkish state: sovereign authority attempting to enforce its preferred interpretation of Lausanne through unilateral legal changes; can modify minority status via legislation
 *   - Greek Orthodox Church: institutionally identity-locked; relies on external (ECtHR, Greek diplomatic) pressure to defend claims Turkey's courts have rejected
 *   - Armenian Apostolic Church: similar to Greek Orthodox but with weaker guarantor backing
 *   - European Court of Human Rights: external adjudicatory seat; issues rulings supporting minority institutional claims but lacks enforcement power
 *   - Greece: primary guarantor state; has leverage via EU conditionality and NATO partnerships; incentivized to portray itself as minority protector
 *   - European Union: amplifies guarantor reading via conditionality on Turkey's accession negotiations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.42).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.31).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections: Guarantor Reading (International Supervision)").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, 'e18940dc-65eb-45f4-9f5a-e78aad86a5c0').
narrative_ontology:cs_kernel_codification('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', fixed_text).
narrative_ontology:cs_authority_grounding('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', extraction).
narrative_ontology:cs_interpretation_layer_present('e18940dc-65eb-45f4-9f5a-e78aad86a5c0').
narrative_ontology:cs_reading_relation('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_reading_relation('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_axiom('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', foundational, lausanne_creates_international_obligations).
narrative_ontology:cs_axiom_status(lausanne_creates_international_obligations, holdable).
narrative_ontology:cs_axiom_grounding('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', lausanne_creates_international_obligations, conventional).
narrative_ontology:cs_axiom('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', foundational, external_supervision_required_for_enforcement).
narrative_ontology:cs_axiom_status(external_supervision_required_for_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', external_supervision_required_for_enforcement, instrumental).
narrative_ontology:cs_reference_frame('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', lausanne_international_supervision_mandate).
narrative_ontology:cs_drift_state('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e18940dc-65eb-45f4-9f5a-e78aad86a5c0', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, religious_minorities_seeking_external_protection).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states_exercising_diplomatic_leverage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, greek_orthodox_church).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, armenian_apostolic_church).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, jewish_community).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, greece_modern_guarantor).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, armenian_state).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, greek_orthodox_church).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, armenian_apostolic_church).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed the Lausanne Treaty in 1923 as a sovereign act, committing to protect religious minorities. Retains domestic legislative and administrative authority to interpret the scope of those protections. Views external supervision as infringement on sovereignty. Can modify minority protections through unilateral legal changes so long as they do not explicitly violate treaty language, and disputes external adjudication as overreach. Faces increasing pressure from European human rights mechanisms and guarantor state diplomacy when minorities petition externally.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, agenda_setter,
    institutional, generational, constrained, national).

% The primary Christian minority in Turkey. Historically held institutional property, theological schools (the Halki Seminary), and administrative autonomy under Ottoman millet system. Since 1923, relies on Lausanne text to claim institutional continuity but Turkish courts consistently read Lausanne narrowly. Can petition the European Court of Human Rights (ECtHR) and appeal to guarantor states (Greece, historically Britain and France) to pressure Turkey diplomatically. Cannot exit: Greek Orthodox identity is constituted through continuity with this institutional tradition. Faces closure of theological schools, property seizures, and administrative subordination to state law.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, greek_orthodox_church, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, greek_orthodox_church, beneficiary).

% Smaller Christian minority with parallel institutional claims under Lausanne. Similarly constrained by restrictive domestic interpretation, similarly identity-locked to institutional continuity. Relies on ECtHR petitions and diplomatic pressure from Armenia (guarantor via post-Soviet succession claim) and diaspora advocacy.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, armenian_apostolic_church, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, armenian_apostolic_church, beneficiary).

% Also protected under Lausanne as a recognized minority. Smaller population than Christian minorities. Faces similar property disputes and administrative subordination. Limited diplomatic backing compared to Greek Orthodox (Greece is a major guarantor; Armenia less so; diaspora states are weaker guarantors).
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, jewish_community, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, jewish_community, beneficiary).

% Interprets the European Convention on Human Rights (ECHR) and reviews petitions from Turkish minorities. Has issued rulings supporting minority institutional and property rights as flowing from Lausanne protections (e.g., Pantelis and others cases). Functions as the primary external adjudication venue under the guarantor reading. Turkey can ignore ECtHR rulings, but non-compliance carries diplomatic cost and can trigger enforcement mechanisms through the Council of Europe.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, continental).

% Primary guarantor state (successor to Lausanne signatories). Can invoke guarantor responsibilities to raise minority protection issues diplomatically with Turkey, support ECtHR cases, and condition EU and NATO cooperation on minority protections. Benefits from portraying itself as protector of Orthodox Christianity. Has leverage through EU membership conditionality and strategic partnerships.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, greece_modern_guarantor, agenda_setter,
    powerful, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, greece_modern_guarantor, beneficiary).

% Claims guarantor role for Armenian Apostolic Church through post-Soviet succession. Has weaker leverage than Greece but can raise the issue through ECHR and diaspora advocacy. Faces constraints from Turkey's regional military dominance.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, armenian_state, agenda_setter,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, armenian_state, beneficiary).

% Does not formally guarantee Lausanne but uses minority protections as a condition for Turkey's accession negotiations. Frames minority rights through human rights mechanisms and rule of law benchmarks. Functions as an amplifier of the guarantor reading via conditionality.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_union, observer,
    institutional, generational, analytical, continental).

% Domestic secular and religious advocacy organizations would support broader minority protections but lack formal voice in Lausanne interpretation and face pressure from state authorities when challenging official readings. Excluded from both the treaty negotiation and the international supervision apparatus.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_civil_society, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, turkish_state).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an international framework for adjudicating minority rights disputes that neither the Turkish state nor the minorities can unilaterally resolve domestically. Routes grievances to external (ECtHR) and diplomatic (guarantor state) channels, creating a check on unilateral state reinterpretation of minority protections.
% TRANSFER_FUNCTION: Does not transfer material goods directly, but allocates adjudicatory authority: moves decision-making about minority protections from exclusive Turkish domestic control to shared international-and-domestic space (ECtHR rulings, guarantor state diplomacy), reducing Turkey's unilateral interpretation power.
% ABSENT_VOICES: Turkish civil society and minority community members themselves are largely excluded from formal Lausanne interpretation — the conversation happens between the Turkish state, guarantor states, and international courts. Minority petitioners to ECtHR are present as complainants but not as architects of the framework.
% DISAPPEARANCE_RATIONALE: If the guarantor reading and its enforcement mechanisms vanished (no more ECtHR jurisdiction over Lausanne claims, no more guarantor state diplomatic pressure), Turkey would face no external constraint on narrowing minority protections further through unilateral legal reinterpretation. Minorities would be left to domestic remedies only, which have consistently rejected institutional and property rights claims. Religious minorities would lose their primary defense mechanism and institutions would face accelerated closure and property seizure.
% FOUNDING_PROBLEM: After the 1923 Treaty of Lausanne, Turkey's unilateral and restrictive legal interpretation of minority protections threatened the continuity of religious institutional life that the treaty was meant to preserve. International supervision through guarantor states and human rights mechanisms was invoked to enforce the treaty's original intent against regressive domestic reinterpretation.
% FOUNDING_PROBLEM_CORROBORATION: ECtHR rulings (Pantelis, Agga, and related cases) document Turkey's pattern of narrowing minority protections through unilateral reinterpretation. Greek and Armenian governments have repeatedly invoked guarantor responsibilities. Legal scholars outside Turkey's orbit (European human rights experts, international law observers) attest that the founding problem persists: Turkey continues to narrow minority rights domestically while relying on treaty language that appears to protect broader institutional claims.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at present): Turkey extracts the benefit of unilateral Lausanne interpretation and narrowing, but the external adjudicatory pathway (ECtHR, guarantor diplomacy) limits how far that extraction can proceed without diplomatic cost. The constraint is claimed as a scaffold (temporary support structure for protecting minorities until a permanent solution is reached), but the measurement pattern shows slow upward drift from 1923 (0.15) to present (0.42) — Turkey has progressively narrowed Lausanne protections over a century, and the external mechanisms have not reversed that drift, only slowed it. Theater ratio (0.48) reflects that Turkey performs compliance with Lausanne while reinterpreting it narrowly; guarantor states perform protection while issuing non-binding ECtHR rulings; the constraint persists partly through staged diplomatic theater rather than functional minority protection. Suppression (0.31) is moderate: Turkey uses legal and administrative measures to suppress institutional claims, but the international attention and ECtHR cases create pressure that keeps suppression from reaching snare levels. Accessibility collapse (0.38) is below mountain levels because the minorities retain the exit option of ECtHR petition and guarantor state appeals — they are not trapped in a purely domestic system, though domestic law strongly constrains their practical options. Resistance (0.72) is high: minorities actively petition ECtHR, guarantor states diplomatically pressure Turkey, and the contest over Lausanne interpretation is live and visible. The constraint's persistence depends on active enforcement of the external pathway, not on naturalness or acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state would compute this constraint as rope or even mountain (a settled international obligation that Turkey has voluntarily adopted and administers domestically). The minorities would compute it as snare (external mechanisms exist but cannot rescue them from Turkish legal restrictions; Turkey can claim compliance with Lausanne while gutting its institutional content). The guarantor states would compute it as tangled rope (coordination function — protecting minorities; extraction function — maintaining diplomatic leverage and EU leadership on human rights). The ECtHR would compute it as rope (genuine coordination function — resolving minority disputes internationally). This seat divergence is structural: the constraint is claimed as a scaffold in this reading (providing temporary external protection until minority status stabilizes), but different seats experience radically different extraction profiles because they have different enforcement leverage and different costs of noncompliance.
 *
 * DIRECTIONALITY LOGIC:
 *   From the Turkish state seat: d ≈ 0.15 (beneficiary); Turkey benefits from the constraint's current form (external mechanisms exist but lack enforcement, so Turkey can absorb the diplomatic cost while maintaining unilateral interpretation power). From the minorities' seat: d ≈ 0.85 (target); they are the structural victims — their institutional claims are denied by Turkey, and external mechanisms provide adjudication but not remedy, leaving them bearing the cost of institutional closure. From the guarantor state seat: d ≈ 0.30 (slight beneficiary); guarantor states benefit from portraying themselves as protectors without having to impose costly economic sanctions on Turkey. From the ECtHR seat: d ≈ 0.50 (symmetric); the court issues rulings (performs its function) but Turkey ignores them (the court's authority is constrained), so the ECtHR neither collects nor pays — it is the structural neutral that makes the game possible but does not profit from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unilateral Turkish reinterpretation narrowing Lausanne protections) is LIVE — Turkey continues to close theological schools, restrict property rights, and subordinate minority institutions to general law. The constraint's mandate is to maintain international supervision of Lausanne interpretation. However, over 103 years, the external mechanisms have not reversed the drift toward restrictive interpretation — they have only slowed it. The theater ratio rising from 0.25 to 0.48 suggests that the constraint's function has increasingly become diplomatic signaling (countries performing commitment to minority rights, ECtHR performing judicial authority) rather than actual protection. The mandatrophy is partial: the founding problem persists; the external mechanisms persist; but the mechanisms have not achieved their stated purpose, and the gap between their authority (to interpret Lausanne) and their enforcement (near-zero if Turkey refuses to comply) has widened. The constraint should be reclassified as piton if the external mechanisms become purely performative — if guarantor states no longer actually use the leverage they claim to have, and ECtHR rulings become decorative. Currently it remains a scaffold because the external pathway is still materially constraining (though insufficiently), and the founding problem is still live enough that the constraint retains justification for existing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_adequacy,
    'Are the external mechanisms (ECtHR rulings, guarantor state diplomacy) sufficient to enforce Lausanne protections, or do they function as diplomatic pressure without binding remedial power?',
    'Empirical observation: track whether Turkey complies with ECtHR rulings (do minorities gain institutional restoration or property restitution?), whether guarantor states impose economic/political costs for non-compliance, and whether Turkey''s legal interpretations shift in response to external pressure.',
    'If external mechanisms remain non-binding, the constraint is a scaffold with decreasing function (and should be reclassified as piton if theater ratio continues rising without remedy). If external mechanisms gain enforcement power (e.g., via EU leverage on accession), the constraint becomes a functioning tangled rope. Current evidence suggests non-binding performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_adequacy, empirical, 'Whether the external enforcement pathway has real remedial power or is purely diplomatic.').

omega_variable(
    reading_demarcation_ambiguity,
    'Does the guarantor reading rest on a structurally distinct understanding of Lausanne''s scope (what the treaty obligates), or is it simply a tactical framing (the same obligations, but enforced internationally rather than domestically)?',
    'Textual and historical analysis: examine whether the guarantor reading''s advocate (Greece, ECtHR, EU) claim that Lausanne text itself requires international supervision, or whether they are proposing an enforcement innovation without changing the underlying obligations.',
    'If the reading is tactical only (same obligations, new enforcement venue), it should be reclassified as snare: minorities still face the same Turkish restrictive interpretation, but now with the added cost of international litigation. If the reading reflects a genuinely different Lausanne interpretation (international supervision is required by treaty text, not merely available as option), the three readings are substantively distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_demarcation_ambiguity, conceptual, 'Whether the reading difference is substantive (different Lausanne interpretation) or tactical (same interpretation, different enforcement venue).').

omega_variable(
    minority_identity_lock_sustainability,
    'How long can the identity-locked minorities (Greek Orthodox, Armenian Apostolic, Jewish) sustain institutional identity and practice if Turkey continues narrowing protections while external mechanisms remain non-binding?',
    'Longitudinal observation: track institutional vitality (seminary enrollment, property holdings, active worship sites, community demographics) over time; conduct exit interviews with minorities considering relocation.',
    'If identity-locked minorities face attrition due to sustained institutional constraints, the constraint shifts from targeting organized communities (role: payer) to targeting individuals (role: payer) with less leverage. The constraint''s extractiveness may rise and the resistance may fall if minorities become too dispersed to mount collective ECtHR cases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_identity_lock_sustainability, empirical, 'Trajectory of minority institutional vitality under sustained restrictive interpretation.').

omega_variable(
    guarantor_state_strategic_commitment,
    'Are guarantor states (Greece, and historically France/UK) actually committed to enforcing Lausanne protections, or do they invoke guarantor responsibilities selectively when strategic interests align?',
    'Historical case analysis: examine whether guarantor states have used their economic or political leverage (EU conditionality, NATO partnerships, trade agreements) to press Turkey on specific minority protection improvements, or merely issued diplomatic protests.',
    'If guarantor states have low actual commitment (invoke responsibilities rhetorically but avoid costly enforcement), the constraint is a pure scaffold or piton — minorities depend on external mechanisms that lack real backing. If guarantor states have higher commitment, the constraint functions as a tangled rope with asymmetric enforcement (minorities seek protection; guarantors enforce at selective times).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guarantor_state_strategic_commitment, empirical, 'Whether guarantor states have genuine enforcement commitment or selective diplomatic interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 1923, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__guarantor_reading, theater_ratio, 1923, 0.25).
narrative_ontology:measurement(laus_tr_t1960, lausanne_minority_protections__guarantor_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__guarantor_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement(laus_tr_t2010, lausanne_minority_protections__guarantor_reading, theater_ratio, 2010, 0.46).
narrative_ontology:measurement(laus_tr_t2020, lausanne_minority_protections__guarantor_reading, theater_ratio, 2020, 0.47).
narrative_ontology:measurement(laus_tr_t2026, lausanne_minority_protections__guarantor_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1923, 0.15).
narrative_ontology:measurement(laus_be_t1960, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(laus_be_t2010, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(laus_be_t2020, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(laus_be_t2026, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1923, 0.15).
narrative_ontology:measurement(laus_su_t1960, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(laus_su_t2010, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(laus_su_t2020, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2020, 0.31).
narrative_ontology:measurement(laus_su_t2026, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2026, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__guarantor_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, turkey_domestic_minority_law).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, echr_jurisdiction_over_treaty_claims).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel is decomposed into three structurally distinct constraints: the expansive_reading (Ottoman institutional continuity, high extraction by restrictive state action), the guarantor_reading (international supervision, moderate extraction due to non-binding mechanisms), and the restrictive_reading (domestic matters only, low extraction because no international oversight). The three readings share the kernel (Lausanne Treaty text) but diverge on what obligations the text creates and who may enforce them. The guarantor_reading bridges the expansive and restrictive readings: it accepts Turkey's domestic authority (rejecting the expansive claim of Ottoman succession) but insists on international verification (rejecting the restrictive claim of pure Turkish sovereignty). The three stories are linked via network.affects_constraints because the reading contest is live — the classification of each reading depends partly on how the other readings are positioned in the public discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
