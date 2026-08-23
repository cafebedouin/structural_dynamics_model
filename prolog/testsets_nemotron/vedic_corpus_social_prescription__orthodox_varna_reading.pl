% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Reading of Vedic Corpus
 *   domain: religious/social/historical
 *
 * SUMMARY:
 *   This constraint story models the orthodox varna reading of the Vedic
 *   corpus: the claim that Vedic texts literally prescribe a fourfold
 *   hereditary hierarchy (Brahmin, Kshatriya, Vaishya, Shudra) as divinely
 *   mandated cosmic order (Rta/Dharma), with a fifth excluded category
 *   (Dalit/untouchable) outside the varna system. The reading treats the
 *   Purusha Sukta (RV 10.90) and subsequent Dharmashastra elaborations as
 *   prescriptive law rather than descriptive cosmology. The constraint
 *   operates through occupational closure, endogamy enforcement, ritual
 *   exclusion, and the ideological apparatus of karma/purity that naturalizes
 *   the hierarchy. Extraction flows as labor value (Shudra service castes,
 *   Dalit polluting labor), ritual monopoly (Brahmin control of
 *   sacrifice/knowledge), and status rents. The kernel
 *   'vedic_corpus_social_prescription' is contested: the
 *   reformist_spiritual_reading denies prescriptive social content entirely;
 *   the colonial_orientalist_reading codifies the hierarchy as fixed Hindu
 *   law for administrative governance. This story generates ONLY the
 *   orthodox_varna_reading as a clean ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - brahmin_caste: Primary beneficiary (institutional/identity_locked) — controls ritual knowledge, temple endowments, and scriptural interpretation; collects status rents and material offerings
 *   - shudra_caste: Primary victim (powerless/trapped) — bound to service occupations, excluded from Vedic study and ritual participation, labor extracted via hereditary occupational closure
 *   - dalit_communities: Primary victim (powerless/trapped) — placed outside varna system entirely, assigned polluting labor, subjected to untouchability practices and spatial segregation
 *   - kshatriya_vaishya_castes: Secondary actors (moderate/constrained) — hold political/economic power but subordinate to Brahmin ritual authority; benefit from hierarchy above Shudras but pay ritual tribute
 *   - reformist_interpreters: Excluded (moderate/mobile) — Bhakti saints, Buddhist/Jain/Sikh traditions, modern reformers (Phule, Ambedkar, Vivekananda) who contest the reading's prescriptive claim
 *   - colonial_administrators: Observer/agenda_setter (institutional/analytical) — codified varna as fixed law for revenue and governance, freezing fluid identities into census categories
 *   - analytical_scholar: Observer (analytical/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.82).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.78).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Reading of Vedic Corpus").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social/historical").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'af60d840-b1b6-40c2-a992-164aeed32f25').
narrative_ontology:cs_kernel_codification('af60d840-b1b6-40c2-a992-164aeed32f25', fixed_text).
narrative_ontology:cs_authority_grounding('af60d840-b1b6-40c2-a992-164aeed32f25', lineage).
narrative_ontology:cs_interpretation_layer_present('af60d840-b1b6-40c2-a992-164aeed32f25').
narrative_ontology:cs_reading_relation('af60d840-b1b6-40c2-a992-164aeed32f25', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('af60d840-b1b6-40c2-a992-164aeed32f25', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('af60d840-b1b6-40c2-a992-164aeed32f25', foundational, varna_hierarchy_divinely_mandated).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('af60d840-b1b6-40c2-a992-164aeed32f25', varna_hierarchy_divinely_mandated, theological).
narrative_ontology:cs_axiom('af60d840-b1b6-40c2-a992-164aeed32f25', foundational, brahmin_epistemic_authority_exclusive).
narrative_ontology:cs_axiom_status(brahmin_epistemic_authority_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('af60d840-b1b6-40c2-a992-164aeed32f25', brahmin_epistemic_authority_exclusive, theological).
narrative_ontology:cs_axiom('af60d840-b1b6-40c2-a992-164aeed32f25', secondary, hereditary_varna_immutable).
narrative_ontology:cs_axiom_status(hereditary_varna_immutable, holdable).
narrative_ontology:cs_axiom_grounding('af60d840-b1b6-40c2-a992-164aeed32f25', hereditary_varna_immutable, theological).
narrative_ontology:cs_axiom('af60d840-b1b6-40c2-a992-164aeed32f25', secondary, ritual_purity_ontologically_real).
narrative_ontology:cs_axiom_status(ritual_purity_ontologically_real, holdable).
narrative_ontology:cs_axiom_grounding('af60d840-b1b6-40c2-a992-164aeed32f25', ritual_purity_ontologically_real, theological).
narrative_ontology:cs_reference_frame('af60d840-b1b6-40c2-a992-164aeed32f25', vedic_sacrificial_cosmology).
narrative_ontology:cs_drift_state('af60d840-b1b6-40c2-a992-164aeed32f25', contemporary_hindutva_mobilization, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('af60d840-b1b6-40c2-a992-164aeed32f25', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_vaishya_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_vaishya_castes).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, varna_dharma_as_cosmic_order).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_ritual_authority).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, ritual_purity_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls Vedic interpretation, ritual performance, temple endowments, and scriptural education. Collects dakshina (ritual fees), land grants, state patronage, and epistemic authority that legitimizes the hierarchy. Brahmin identity is constituted through the hierarchy — leaving it means ceasing to be Brahmin structurally. Sub-groups (ritual specialists, landholders, scribes) capture different rents but share the identity lock.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary,
    institutional, generational, identity_locked, continental).

% Bound to hereditary service occupations (artisans, laborers, servants) serving the three higher varnas. Excluded from Vedic study, upanayana (sacred thread), and ritual participation. Labor value extracted via occupational closure and ritual subordination. Legal exit exists (constitutional rights, conversion) but structural exit blocked by endogamy enforcement, economic dependency, and internalized karma ideology that frames subordination as just desert.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, biographical, trapped, continental).

% Placed outside the varna system entirely (avarna/untouchable). Assigned polluting labor (scavenging, leather, cremation) and subjected to untouchability practices: spatial segregation, denial of water/temple access, violence for boundary transgression. Extractive intensity exceeds Shudra — not just service but pollution absorption. Exit is trapped: conversion movements (Buddhism, Christianity, Ambedkarite) break the identity frame but face violent backlash; constitutional protections exist but enforcement is captured by dominant castes.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, biographical, trapped, continental).

% Hold political power (Kshatriya) and economic power (Vaishya) but subordinate to Brahmin ritual authority. Benefit from hierarchy above Shudras/Dalits — control land, trade, administration — but pay ritual tribute (dakshina, patronage) and accept Brahmin superiority as cosmic order. Exit is constrained: can contest Brahmin dominance (historical Kshatriya-Brahmin conflicts) but cannot reject the hierarchy itself without losing their own positional rents. Some sub-groups align with reformist movements for strategic advantage.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_vaishya_castes, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_vaishya_castes, payer).

% Bhakti saints (Basava, Kabir, Ravidas, Mirabai), Buddhist/Jain/Sikh traditions, modern reformers (Phule, Ambedkar, Vivekananda, Periyar). Contest the reading's prescriptive claim: argue Vedic texts describe spiritual unity, not social hierarchy; or that hierarchy is a later corruption. Structurally excluded from authoritative interpretation circuit — their readings are marginalized as heterodox, their institutions denied state recognition. Exit is mobile: they build alternative communities and interpretive traditions, but cannot displace the orthodox reading's institutional capture.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_interpreters, excluded,
    moderate, biographical, mobile, continental).

% British colonial state codified varna as fixed 'Hindu law' for revenue collection, judicial administration, and census classification (1871 onward). Froze fluid, contextual identities (jati, gotra, regional variation) into four rigid varna categories. Benefited from the hierarchy as administrative infrastructure — Brahmin intermediaries facilitated rule. Exit is arbitrage: the colonial state could (and did) reshape the constraint for governance, but the reading it produced (colonial_orientalist_reading) became a sibling constraint that persists post-independence.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Sees the full structure across readings: the kernel's textual ambiguity, the three readings' structural divergence, the extraction flows, the suppression mechanisms. Neither collects nor pays; provides the classification surface. Exit is analytical — the seat exists only in the analysis.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, analytical_scholar, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, hereditary division of labor and ritual function across a vast, diverse subcontinent — solves the coordination problem of social order without a centralized bureaucratic state. The four varnas (plus excluded fifth) allocate occupational, ritual, and status roles in a self-reproducing system.
% TRANSFER_FUNCTION: Moves labor value (Shudra service, Dalit polluting labor), ritual fees and offerings (dakshina, dana), land grants (brahmadeya, agrahara), and epistemic authority (Vedic interpretation, teaching rights) from Shudra/Dalit/Kshatriya/Vaishya to Brahmin caste. Status deference flows upward; pollution burden flows downward.
% ABSENT_VOICES: Shudra and Dalit voices are structurally excluded from the authoritative interpretive circuit — the very texts that prescribe their subordination deny them Vedic study and ritual speech. Bhakti and anti-caste traditions (Buddhism, Sikhism, Ambedkarism) are the excluded voices that would object; they exist but are kept out of the orthodox reading's authority structure. Women of all varnas are doubly excluded — from Vedic study and from independent ritual agency.
% DISAPPEARANCE_RATIONALE: If the orthodox varna reading vanished overnight, the hereditary occupational closure, endogamy enforcement, ritual exclusion, and untouchability practices would lose their cosmic justification. The material hierarchy (land, labor, temple control) would persist initially but its ideological armor would be gone — conversion movements, inter-caste marriage, and democratic politics would reorganize social relations rapidly. The world rearranges because arrangements depend on it: the constraint is the legitimating ideology of a material extraction system.
% FOUNDING_PROBLEM: Early Vedic society faced the coordination problem of integrating diverse tribal groups into a stable social order with functional specialization (priest, warrior, producer, servant) without a centralized state. The varna framework provided a cosmic template for this division, legitimated by the Purusha Sukta's sacrificial cosmology.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (stateless functional coordination) is dead — modern India has a centralized bureaucratic state, market economy, and constitutional democracy that solve coordination differently. The arrangement persists as extraction. Corroboration from outside beneficiaries: Ambedkar (Dalit leader, constitutional architect) attested the founding problem is dead and the arrangement is extraction; Phule (Shudra reformer) documented the hierarchy as Aryan conquest imposed on indigenous populations; Marxist historians (Kosambi, Sharma) analyze varna as class formation, not cosmic order; Buddhist texts (early, outside Brahmin tradition) describe varna as social convention, not divine law. No corroborating source outside the Brahmin tradition attests the founding problem as live.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) reflects the scale of labor value extraction (Shudra service castes, Dalit polluting labor), ritual monopoly rents (Brahmin control of sacrifice, education, legitimacy), and status rents across ~2500 years. The slight dip at 1950 (0.65) captures the constitutional abolition of untouchability and affirmative action — formal legal extraction drops but the structural hierarchy persists. The rise back to 0.82 by 2025 captures the resurgence of orthodox assertion in contemporary politics, where the reading is weaponized for majoritarian mobilization. Suppression (0.78) is high: the constraint's persistence depends on active enforcement of endogamy, occupational barriers, and ritual exclusion, plus the internalized suppression of karma/purity ideology. Theater ratio (0.25) is moderate-low: the ritual/ideological apparatus has genuine coordination functions (social stability, knowledge transmission) but a growing share of activity defends the hierarchy's material benefits. Accessibility collapse (0.68) reflects that alternatives (Buddhism, Bhakti, Sikhism, constitutional equality) exist but are structurally marginalized or co-opted. Resistance (0.55) is substantial: continuous counter-traditions from Shramanic movements through Bhakti to Ambedkarite politics, but the constraint survives by absorbing and domesticating dissent.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin seat experiences this as a Mountain or Rope — divine order that coordinates society and preserves knowledge. The Shudra/Dalit seats experience it as a Snare — enforced extraction with identity-locked exit. The Kshatriya/Vaishya seats experience it as a Tangled Rope — they coordinate political/economic power but pay ritual tribute and accept Brahmin superiority. The reformist seat sees a false summit: a constructed hierarchy wearing Mountain's clothes. The colonial seat sees a Scaffold they mistakenly treated as a Mountain — they codified fluid practice into fixed law. The engine computes this divergence from the structural data; the authored claim (snare) reflects the victim seats' structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin_caste is the structural beneficiary (d ~ 0.15): collects ritual fees, dakshina, land grants, state patronage, and epistemic authority; controls the interpretation that legitimizes the hierarchy. Exit is identity_locked — Brahmin identity is constituted through the hierarchy; leaving it means ceasing to be Brahmin in the structural sense. Shudra_caste and dalit_communities are structural victims (d ~ 0.9): bear hereditary occupational closure, ritual exclusion, spatial segregation, and the ideological burden of pollution. Exit is trapped — legal exit exists (constitutional rights, conversion) but structural exit is blocked by endogamy enforcement, economic dependency, and internalized karma ideology. Kshatriya/Vaishya are symmetric (d ~ 0.5): gain status above Shudras but subordinate to Brahmins; their exit is constrained — they can contest Brahmin dominance but not the hierarchy itself without losing their own position. Reformist_interpreters are excluded (d ~ 0.7): they would object but are structurally kept out of the authoritative interpretation circuit. Colonial_administrators are agenda_setters for a period (d ~ 0.3): they benefit from the hierarchy as administrative infrastructure but do not originate it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (social coordination via functional division) is dead — modern economy and state have other coordination mechanisms. The arrangement persists as extraction: Brahmin ritual monopoly and status rents, Shudra/Dalit labor extraction. The mandate (cosmic order) has atrophied into a snare. The theater ratio rise after 1850 reflects the constraint's shift from coordination to extraction maintenance: colonial codification froze it, postcolonial politics reactivated it. The high resistance (0.55) is not enough to break it because the beneficiary captures the epistemic authority that defines what counts as legitimate resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''vedic_corpus_social_prescription'', and what does the sibling reading ''reformist_spiritual_reading'' change structurally?',
    'Committee analysis of reading relations: the reformist reading denies prescriptive social content entirely, removing the victim set and collapsing extraction to near-zero; the colonial reading codifies the hierarchy as fixed law for administration, shifting the agenda_setter to a colonial state apparatus.',
    'If the reformist reading is structurally viable, this reading''s high extractiveness is not intrinsic to the kernel but a property of this specific framing — the kernel does not uniquely determine ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee frame: this reading vs. sibling readings of the same kernel').

omega_variable(
    varna_ontology_ambiguity,
    'Does the Vedic corpus prescribe a fixed hereditary hierarchy (varna as birth-determined) or describe a functional/qualificational division (varna as guna-karma based)?',
    'Philological comparison of Vedic vs. Dharmashastra textual strata; historical analysis of when hereditary closure became dominant.',
    'If varna is originally qualificational, the hereditary hierarchy is a later accretion — this reading projects Dharmashastra closure back onto Vedic text, inflating extraction by naturalizing a constructed boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_ontology_ambiguity, conceptual, 'Textual ontology: prescriptive hereditary hierarchy vs. descriptive functional division').

omega_variable(
    coercion_mechanism_ambiguity,
    'Is the suppression of Shudra/Dalit exit structural (state enforcement, economic dependency, ritual exclusion) or internalized (identity fusion with the hierarchy, belief in karmic justice)?',
    'Post-exit trajectory study: if suppression persists after legal emancipation and economic mobility, the internalized component is significant; compare conversion movements (Buddhism, Bhakti, Christianity, Ambedkarite) where identity frame breaks.',
    'If substantially internalized, effective suppression exceeds the structural measure — the constraint travels with the agent after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for lower varna exit').

omega_variable(
    beneficiary_scope_ambiguity,
    'Does the Brahmin caste uniformly benefit, or do sub-groups (ritual specialists vs. landholding elites vs. state-servicing scribes) capture different rents?',
    'Historical political economy of Brahmin sub-castes across regions and periods; analysis of who controls temple endowments, land grants, and administrative posts.',
    'If benefits are concentrated in a sub-group, the beneficiary declaration ''brahmin_caste'' aggregates distinct structural positions — the extraction flow may be narrower than the declared beneficiary set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_scope_ambiguity, empirical, 'Internal differentiation of the declared beneficiary group').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 500, 0.18).
narrative_ontology:measurement_basis(vedi_tr_t500, observed).
narrative_ontology:measurement(vedi_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement_basis(vedi_tr_t1000, observed).
narrative_ontology:measurement(vedi_tr_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t1500, observed).
narrative_ontology:measurement(vedi_tr_t1850, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1850, 0.3).
narrative_ontology:measurement_basis(vedi_tr_t1850, observed).
narrative_ontology:measurement(vedi_tr_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(vedi_tr_t1950, observed).
narrative_ontology:measurement(vedi_tr_t2025, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 500, 0.72).
narrative_ontology:measurement_basis(vedi_be_t500, observed).
narrative_ontology:measurement(vedi_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.78).
narrative_ontology:measurement_basis(vedi_be_t1000, observed).
narrative_ontology:measurement(vedi_be_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1500, 0.82).
narrative_ontology:measurement_basis(vedi_be_t1500, observed).
narrative_ontology:measurement(vedi_be_t1850, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1850, 0.8).
narrative_ontology:measurement_basis(vedi_be_t1850, observed).
narrative_ontology:measurement(vedi_be_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement_basis(vedi_be_t1950, observed).
narrative_ontology:measurement(vedi_be_t2025, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement_basis(vedi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 500, 0.62).
narrative_ontology:measurement_basis(vedi_su_t500, observed).
narrative_ontology:measurement(vedi_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement_basis(vedi_su_t1000, observed).
narrative_ontology:measurement(vedi_su_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1500, 0.75).
narrative_ontology:measurement_basis(vedi_su_t1500, observed).
narrative_ontology:measurement(vedi_su_t1850, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement_basis(vedi_su_t1850, observed).
narrative_ontology:measurement(vedi_su_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement_basis(vedi_su_t1950, observed).
narrative_ontology:measurement(vedi_su_t2025, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement_basis(vedi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.1).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, dharmashastra_codification).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_census_classification).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, postcolonial_reservation_policy).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, hindutva_mobilization).

% DUAL FORMULATION NOTE:
% Constraint family: vedic_corpus_social_prescription kernel with three readings. This reading (orthodox_varna) has high ε (0.82) because it enforces hereditary hierarchy as divine law. reformist_spiritual_reading has near-zero ε (denies prescriptive content). colonial_orientalist_reading has high ε but beneficiary is colonial state apparatus, not Brahmin caste. The readings are linked by affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, institutional, 0.15).
constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, powerless, 0.9).
constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
