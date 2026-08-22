% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Syncretic Unity: Kami as Buddhist Manifestations
 *   domain: religious_studies/ontology_of_practice
 *
 * SUMMARY:
 *   This story instantiates the syncretic reading of the shinbutsu
 *   ontological commitment: kami and buddhas are held to be aspects of one
 *   unified cosmological order, with kami as suijaku (traces/manifestations)
 *   of buddhas or bodhisattvas as honji (original ground). This is the
 *   dominant institutional reading from roughly the Heian period through the
 *   Edo period, embedded in jingu-ji (combined shrine-temple) administration,
 *   doctrinal treatises (e.g. Ryobu Shinto, Sanno Shinto), and
 *   court-sponsored ritual calendars. The reading treats the unity claim as
 *   metaphysically real, not as a diplomatic fiction — which is precisely
 *   what makes it structurally different from the partition_reading (separate
 *   domains, no ontological fusion) and the incoherence_reading (no stable
 *   commitment existed at all, just tolerated ambiguity). Under this reading
 *   specifically, the doctrinal coherence is real and load-bearing, and it is
 *   this genuine coherence that channels authority toward the Buddhist
 *   hierarchy that supplies and administers the unifying metaphysics.
 *
 * KEY AGENTS:
 *   - buddhist_temple_hierarchy: primary beneficiary/agenda_setter (institutional/arbitrage) — administers the honji-suijaku correspondences and captures land, tribute, and interpretive authority
 *   - shingon_tendai_doctrinal_schools: beneficiary (institutional/arbitrage) — supplies the metaphysical apparatus, gains prestige and ordination authority
 *   - independent_kami_cult_lineages: primary target (moderate/constrained) — their cosmology is subordinated to a borrowed explanatory frame
 *   - local_shrine_priests: primary target (powerless/trapped) — retain ritual practice but lose interpretive authority over its meaning
 *   - later_kokugaku_shinto_scholars: excluded — absent from the honji-suijaku era's own conversation, later contest the framing entirely
 *   - comparative_religion_scholars: analytical observer — assesses whether the unity claim is genuine integration or hierarchy-preserving redescription
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.58).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.62).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Unity: Kami as Buddhist Manifestations").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '01d1926b-6ea2-472e-bcfa-e6700e381451').
narrative_ontology:cs_kernel_codification('01d1926b-6ea2-472e-bcfa-e6700e381451', distributed).
narrative_ontology:cs_authority_grounding('01d1926b-6ea2-472e-bcfa-e6700e381451', lineage).
narrative_ontology:cs_interpretation_layer_present('01d1926b-6ea2-472e-bcfa-e6700e381451').
narrative_ontology:cs_reading_relation('01d1926b-6ea2-472e-bcfa-e6700e381451', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('01d1926b-6ea2-472e-bcfa-e6700e381451', shinbutsu_ontological_commitment__incoherence_reading, influences).
narrative_ontology:cs_axiom('01d1926b-6ea2-472e-bcfa-e6700e381451', foundational, kami_and_buddhas_share_single_ontological_ground).
narrative_ontology:cs_axiom_status(kami_and_buddhas_share_single_ontological_ground, holdable).
narrative_ontology:cs_axiom_grounding('01d1926b-6ea2-472e-bcfa-e6700e381451', kami_and_buddhas_share_single_ontological_ground, conventional).
narrative_ontology:cs_axiom('01d1926b-6ea2-472e-bcfa-e6700e381451', secondary, buddhas_constitute_explanatory_honji_of_kami).
narrative_ontology:cs_axiom_status(buddhas_constitute_explanatory_honji_of_kami, holdable).
narrative_ontology:cs_axiom_grounding('01d1926b-6ea2-472e-bcfa-e6700e381451', buddhas_constitute_explanatory_honji_of_kami, conventional).
narrative_ontology:cs_reference_frame('01d1926b-6ea2-472e-bcfa-e6700e381451', heian_period_jingu_ji_doctrinal_consolidation).
narrative_ontology:cs_drift_state('01d1926b-6ea2-472e-bcfa-e6700e381451', meiji_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('01d1926b-6ea2-472e-bcfa-e6700e381451', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, shingon_tendai_doctrinal_schools).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, court_sponsored_temple_shrine_complexes).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, independent_kami_cult_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_shrine_priests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, lay_pilgrims_and_villagers).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, lay_pilgrims_and_villagers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the honji-suijaku doctrinal apparatus through temple-shrine complexes (jingu-ji), determines which kami correspond to which buddha or bodhisattva as honji (original ground), and controls the ritual calendar, land endowments, and priestly appointments that follow from this correspondence. Collects tribute, land grants, and ritual authority by positioning Buddhist metaphysics as the explanatory ground of kami worship.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy, beneficiary).

% Supplies the metaphysical vocabulary (honji-suijaku, hongaku thought) that makes the unified cosmology intellectually coherent and portable across regions. Gains prestige, ordination authority, and control over the interpretive apparatus that decides what a given kami 'really is' in Buddhist terms.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shingon_tendai_doctrinal_schools, beneficiary,
    institutional, civilizational, arbitrage, national).

% Jointly administered institutions (like Iwashimizu Hachimangu or the Tonomine complex) that draw legitimacy and revenue from presenting kami veneration and Buddhist ritual as continuous expressions of one order, which stabilizes court patronage and pilgrimage income.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, court_sponsored_temple_shrine_complexes, beneficiary,
    institutional, generational, constrained, regional).

% Local kami traditions with their own priestly lineages, purity codes, and cosmological accounts predating or independent of Buddhist integration. Under the syncretic reading their kami are redescribed as provisional manifestations (suijaku) of a Buddhist honji, which subordinates their own cosmology to a borrowed explanatory frame and channels their symbolic authority upward into the temple hierarchy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, independent_kami_cult_lineages, payer,
    moderate, generational, constrained, regional).

% Perform kami ritual at shrines increasingly co-administered or doctrinally subordinated to a nearby temple. Their ritual practice continues materially unchanged, but their authority to explain what they are doing is displaced onto Buddhist clergy and doctrine; leaving the arrangement means losing institutional recognition, land support, and pilgrimage traffic tied to the jingu-ji complex.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_shrine_priests, payer,
    powerless, biographical, trapped, local).

% Receive a coherent, single explanatory framework that lets them worship kami and buddhas without perceived contradiction, and gain access to combined ritual calendars and pilgrimage networks. They also absorb whatever doctrinal hierarchy is embedded in the framework without much capacity to contest its terms.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, lay_pilgrims_and_villagers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, lay_pilgrims_and_villagers, payer).

% Emerge centuries later (Edo period) arguing that the syncretic framing was always a Buddhist imposition on an autonomous native tradition. They are excluded from the honji-suijaku era's own institutional conversation but retroactively contest its ontological claims once political conditions (Meiji shinbutsu bunri) permit.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, later_kokugaku_shinto_scholars, excluded,
    organized, civilizational, analytical, national).

% Analyze whether honji-suijaku represents genuine doctrinal integration, institutionally tolerated ambiguity, or a functional partition dressed in unifying language. Draw on temple records, shrine registers, and doctrinal treatises without a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent cosmological vocabulary that lets kami veneration and Buddhist practice be performed at the same sites, by overlapping personnel, under one interpretive scheme — avoiding the friction of two parallel, potentially conflicting sacred economies operating in the same villages and courts.
% TRANSFER_FUNCTION: Moves interpretive authority, land endowments, and ritual precedence from independent kami lineages and local shrine priests toward Buddhist temple hierarchies and doctrinal schools, who become the parties authorized to say what a kami ultimately is.
% ABSENT_VOICES: Local kami priests and independent cult lineages had little say in how their kami were assigned honji correspondences; centuries later, Kokugaku scholars and Meiji-era Shinto nationalists would object loudly to the entire framing, but they are not present as parties within the honji-suijaku institutional order itself.
% DISAPPEARANCE_RATIONALE: If the syncretic ontological commitment vanished, jingu-ji complexes would lose their doctrinal justification for joint administration, land grants tied to combined ritual calendars would be renegotiated, and local shrine priests would regain unmediated authority over what their kami mean — which is approximately what happened at Meiji shinbutsu bunri when the state forcibly separated the traditions.
% FOUNDING_PROBLEM: Buddhism arriving in Japan needed to explain its relationship to pre-existing, deeply entrenched kami worship without triggering wholesale rejection; honji-suijaku solved this by making kami intelligible as expedient local manifestations of universal buddhas, permitting coexistence instead of confrontation.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist temple chronicles and doctrinal treatises attest the problem as genuinely solved through coherent metaphysics. Independent shrine records and later Kokugaku textual scholarship (outside the Buddhist-benefiting institutions) attest that the 'unification' was substantially a hierarchy-preserving redescription imposed from the more institutionally resourced side, corroborated by the ease and speed of the Meiji-era forced separation, which suggests the underlying traditions remained separable rather than truly fused.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.62) sit in the moderate-substantial range because the syncretic reading, taken on its own terms, does real coordination work — it genuinely resolves what would otherwise be doctrinal friction between two sacred economies sharing the same population and often the same sites. That coordination function is real, which is why this is authored as tangled_rope rather than snare: there is a genuine coordination benefit (lay pilgrims get a coherent worship framework; temple-shrine complexes avoid institutional conflict) running alongside asymmetric extraction (interpretive and material authority flows upward to the Buddhist hierarchy that supplies the unifying metaphysics). Theater ratio (0.40) reflects that a substantial share of the apparatus — elaborate correspondence tables assigning specific buddhas to specific kami, ornate doctrinal treatises — functions partly as legitimating performance for a hierarchy that could, in principle, coordinate ritual calendars without claiming full ontological fusion. The rising suppression_requirement trajectory (0.30 to 0.62) models the deepening of institutional enforcement mechanisms (jingu-ji administrative integration, doctrinal orthodoxy enforcement against rival cosmological accounts) as the syncretic framework matured from an early explanatory convenience into an entrenched administrative order that required active defense against both popular divergent practice and, eventually, revisionist Shinto scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist temple hierarchy's seat, the syncretic order is coordination they built and maintain — a genuine metaphysical achievement resolving what would otherwise be two competing sacred claims on the same population. From the seat of an independent kami lineage or a local shrine priest, the same structure operates as their tradition being redescribed as derivative and provisional, with the 'unity' running in one direction only: kami are explained by buddhas, never buddhas by kami. The engine computes this divergence from the differing power/exit structural data; this story does not adjudicate which seat is correct, only records that the syncretic reading itself treats the redescription as real metaphysical discovery.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temple hierarchy and doctrinal schools sit at the beneficiary end: institutional power, arbitrage-grade exit (they can reposition doctrinally or administratively with little cost), and they are the parties who set the terms of the honji correspondences. Independent kami lineages and local shrine priests sit toward the target end: their cosmological and interpretive autonomy is what gets absorbed into the unified order, and their exit options are constrained-to-trapped because leaving the jingu-ji arrangement forfeits land support and pilgrimage income. Lay pilgrims sit closer to symmetric — real coordination benefit (a usable, non-contradictory worship framework) alongside a diffuse cost (absorbing whatever hierarchy is embedded in the framework without much capacity to contest it).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this as pure extraction (snare) by preserving the genuine coordination function: two large sacred economies with overlapping populations genuinely needed *some* way to coexist without triggering rejection or violence, and honji-suijaku metaphysics provided a real, intellectually serious answer that many practitioners at all levels seem to have found sincerely satisfying, not merely imposed. At the same time it prevents mislabeling this as pure coordination (rope) by keeping in view that the coordination consistently ran through a hierarchy that assigned Buddhist concepts explanatory priority over kami concepts, never the reverse, and that this asymmetry required active administrative and doctrinal enforcement (jingu-ji governance, orthodoxy maintenance) to hold — exactly the requires_active_enforcement condition the tangled_rope gate tests for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_reading_selection_warrant,
    'What in the historical record specifically supports treating honji-suijaku as a genuine, load-bearing metaphysical commitment rather than institutionally tolerated ambiguity (incoherence_reading) or a functional division without ontological fusion (partition_reading)?',
    'Close reading of doctrinal treatises (e.g. Ryobu Shinto texts, Sanno Shinto materials) for internal consistency and sincerity markers, cross-referenced against administrative records showing whether jingu-ji governance actually enforced doctrinal correspondence or merely tolerated coexistence; comparison with cases of documented practitioner confusion or contradiction that would support the incoherence_reading instead.',
    'If the record better supports institutionally tolerated ambiguity, this story''s claimed_type and beneficiary structure would need to migrate toward the incoherence_reading''s profile (likely lower doctrinal-coherence-driven extraction, more diffuse/unintentional asymmetry). If the record shows separable functional domains rather than fused ontology, the partition_reading''s profile (likely lower suppression, since no unified hierarchy needs defending) would be the better fit. This omega documents that the reading choice is itself a live scholarly contest, not a settled fact this story adjudicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_reading_selection_warrant, conceptual, 'Whether the syncretic (unified-ontology) reading is the best-supported account of honji-suijaku relative to its partition and incoherence siblings.').

omega_variable(
    meiji_separation_as_evidence,
    'Does the relative ease and speed of the Meiji-era shinbutsu bunri (forced separation) count as evidence against the syncretic reading''s claim of genuine ontological fusion?',
    'Historical analysis of how quickly and cleanly shrines and temples were administratively separated in 1868 onward, and whether local communities experienced this as restoring a suppressed distinction or as an artificial imposition destroying a real unity.',
    'Easy, rapid separation with minimal popular resistance to the *separation itself* (as opposed to resistance to iconoclasm/violence) would favor the partition_reading or incoherence_reading over this story''s syncretic_reading — suggesting the traditions were never as fused as honji-suijaku doctrine claimed. Difficult, contested separation with genuine cosmological confusion among practitioners would support this story''s premise that real ontological fusion had occurred and was being undone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_evidence, empirical, 'Whether the historical separability of the traditions under Meiji policy bears on the truth of the syncretic reading''s fusion claim.').

omega_variable(
    sincerity_vs_hierarchy_preservation,
    'Among the Buddhist doctrinal schools that authored honji-suijaku metaphysics, how much of the correspondence scheme was driven by sincere metaphysical reasoning versus deliberate hierarchy preservation?',
    'Comparative analysis of correspondence assignments (which kami get which honji) for patterns that track political/economic convenience versus patterns that track internally consistent doctrinal reasoning independent of institutional interest.',
    'High correlation between correspondence assignments and the economic/political interests of assigning institutions would strengthen the tangled_rope classification''s asymmetric-extraction leg; low correlation (correspondences driven by doctrinal logic alone, sometimes against institutional interest) would push the classification toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sincerity_vs_hierarchy_preservation, empirical, 'Whether the specific kami-buddha correspondences track institutional interest or independent doctrinal reasoning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 150, 0.25).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 300, 0.3).
narrative_ontology:measurement(shin_tr_t450, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 450, 0.35).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 600, 0.37).
narrative_ontology:measurement(shin_tr_t750, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 750, 0.39).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 150, 0.42).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 300, 0.5).
narrative_ontology:measurement(shin_be_t450, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 450, 0.55).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 600, 0.56).
narrative_ontology:measurement(shin_be_t750, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 750, 0.57).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 150, 0.38).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 300, 0.48).
narrative_ontology:measurement(shin_su_t450, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 450, 0.55).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 600, 0.58).
narrative_ontology:measurement(shin_su_t750, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 750, 0.6).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__syncretic_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the shinbutsu_ontological_commitment kernel. The partition_reading (separate life-cycle/afterlife domains, no ontological fusion) and incoherence_reading (no stable commitment, tolerated ambiguity) are sibling constraints authored separately, each with its own ε, beneficiary/victim structure, and claimed_type — per the ε-invariance principle, the differing structural claims about whether fusion is real, partial, or absent are not reconciled into a single story with a measurement parameter. This story's ε (0.58) reflects genuine doctrinal-coherence-driven coordination plus real asymmetric extraction; the sibling readings would author materially different ε values reflecting their different premises about what honji-suijaku actually was.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__syncretic_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
