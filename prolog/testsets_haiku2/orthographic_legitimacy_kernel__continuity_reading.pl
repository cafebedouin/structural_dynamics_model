% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Continuity: Historical and Religious Access via Script Preservation
 *   domain: political_linguistics/state_formation/cultural_preservation
 *
 * SUMMARY:
 *   This constraint is the CONTINUITY READING of the orthographic legitimacy
 *   kernel — the claim that legitimacy derives from preserving access to
 *   historical, religious, and literary tradition carried in the old script.
 *   The standing arrangement under contest is post-reform society's
 *   structural severing of newly literate cohorts from pre-reform texts due
 *   to script incompatibility. This reading frames script preservation as a
 *   natural barrier (physical fact: one script cannot be read by those who
 *   know only another script) with inequitable consequences: access to the
 *   cultural record becomes gatekept by institutions and specialists.
 *   Orthographic incompatibility is a physical constraint (a mountain-like
 *   fact), but its institutional deployment — who teaches which script, who
 *   controls which texts, what is framed as 'authentic' tradition — creates
 *   extractive dynamics. The constraint is claimed as a MOUNTAIN (script
 *   incompatibility is physical and emerges naturally from linguistic
 *   structure), but the metrics reflect the extractive institutional dynamics
 *   layered on top (suppression 0.62: the constraint is maintained by
 *   controlling educational curriculum and keeping the old script out of
 *   public schools; beneficiaries named: religious scholars and text
 *   custodians profit from gatekeeping; victims named: post-reform
 *   generations severed from direct access). This is a FALSE SUMMIT candidate
 *   — a natural-law claim with identifiable beneficiaries — which triggers
 *   FSM evaluation.
 *
 * KEY AGENTS:
 *   - post_reform_generations: newly literate cohorts who cannot read pre-reform texts (powerless / trapped exit) — bear the cost of severance
 *   - religious_scholars: maintain interpretive monopoly over Quranic and hadith texts (organized / arbitrage exit) — benefit from gatekeeping
 *   - pre_reform_text_custodians: institutional holders of original manuscripts (powerful / mobile exit) — benefit from indispensability
 *   - arabic_script_preservationists: ideological and institutional agenda-setters defending script legitimacy (organized / mobile exit) — frame and defend the constraint
 *   - newly_literate_cohorts: first-generation readers trapped in new-script-only education (powerless / trapped exit) — bear cumulative exclusion
 *   - modernist_state_architects: institutional observers who imposed the reform (analytical seat) — do not attend to the access consequences
 *   - translation_and_transmission_infrastructure: institutional beneficiary (non-agent) — profits from mediation demand
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.38).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.62).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Continuity: Historical and Religious Access via Script Preservation").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/cultural_preservation").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '85bb24c8-5858-4fcf-b43d-86f897c085c5').
narrative_ontology:cs_kernel_codification('85bb24c8-5858-4fcf-b43d-86f897c085c5', fixed_text).
narrative_ontology:cs_authority_grounding('85bb24c8-5858-4fcf-b43d-86f897c085c5', lineage).
narrative_ontology:cs_interpretation_layer_present('85bb24c8-5858-4fcf-b43d-86f897c085c5').
narrative_ontology:cs_reading_relation('85bb24c8-5858-4fcf-b43d-86f897c085c5', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('85bb24c8-5858-4fcf-b43d-86f897c085c5', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('85bb24c8-5858-4fcf-b43d-86f897c085c5', foundational, script_preserves_interpretive_lineage).
narrative_ontology:cs_axiom_status(script_preserves_interpretive_lineage, holdable).
narrative_ontology:cs_axiom_grounding('85bb24c8-5858-4fcf-b43d-86f897c085c5', script_preserves_interpretive_lineage, deontological).
narrative_ontology:cs_axiom('85bb24c8-5858-4fcf-b43d-86f897c085c5', secondary, direct_access_to_original_texts_is_cultural_right).
narrative_ontology:cs_axiom_status(direct_access_to_original_texts_is_cultural_right, holdable).
narrative_ontology:cs_axiom_grounding('85bb24c8-5858-4fcf-b43d-86f897c085c5', direct_access_to_original_texts_is_cultural_right, deontological).
narrative_ontology:cs_reference_frame('85bb24c8-5858-4fcf-b43d-86f897c085c5', textual_continuity_through_preserved_script).
narrative_ontology:cs_drift_state('85bb24c8-5858-4fcf-b43d-86f897c085c5', contemporary_post_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('85bb24c8-5858-4fcf-b43d-86f897c085c5', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, arabic_script_preservationists).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, religious_scholars).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, pre_reform_text_custodians).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, newly_literate_cohorts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born after orthographic reform, they gain literacy in the new script but cannot read pre-1928 religious texts, classical poetry, administrative records, or historical documents without learning the old script as a second system. They carry the cumulative loss of unmediated access to centuries of cultural production. Their educational pathway is locked into the new script; exit into biliteracy requires active costly effort outside the institutional curriculum.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    moderate, biographical, identity_locked, national).

% Maintain exclusive interpretive authority over pre-reform Islamic texts and historical religious sources. The script incompatibility creates a functional monopoly on reading the Quran and hadith in their original forms. They can arbitrage between those who seek direct access (students, clergy) and those who must rely on their mediation or translation. Their professional identity and institutional power depend on preserving this interpretive gatekeeping.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_scholars, beneficiary,
    organized, generational, arbitrage, national).

% Institutions (libraries, state archives, religious foundations) that hold original manuscripts and historical documents in the old script. They control access and interpretation. Script incompatibility makes their stewardship seem indispensable: the old texts cannot be read without institutional mediation or dual-script education. They collect prestige, funding, and legitimacy from being the keepers of inaccessible cultural patrimony.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, pre_reform_text_custodians, beneficiary,
    powerful, generational, mobile, national).

% Religious and cultural movements, institutional actors, and intellectual factions who actively resist orthographic reform or defend the legitimacy of script preservation. They administer the ideological frame that connects script to religious authenticity and cultural continuity. They maintain schools, publishing houses, and advocacy networks to sustain Arabic-script literacy and frame the new script as rupture from legitimate tradition.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, arabic_script_preservationists, agenda_setter,
    organized, generational, mobile, national).

% First-generation readers taught exclusively in the new script. They experience literacy as access to contemporary administration and commerce but face an invisible barrier to pre-reform texts. They cannot independently assess what knowledge or cultural goods existed before reform; they depend entirely on institutional translation, summary, or formal education to access it. Their exclusion is complete unless they invest years of additional learning.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, newly_literate_cohorts, payer,
    powerless, immediate, trapped, regional).

% Government actors and policy makers who framed orthographic reform as a modernization project, rupture from the Ottoman/Islamic past, and alignment with European nation-states. They imposed the new script through education policy and administrative adoption. They take an analytical stance on the consequence (generational script divide) but may not attend to it as a structural harm, instead viewing it as the price of progress.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, modernist_state_architects, observer,
    institutional, generational, analytical, national).

% The institutional apparatus (academic departments, translation publishers, scholarly institutes) that mediates access to pre-reform texts. It is not an agent but a structural beneficiary: the constraint creates demand for translators, academic specialization in Ottoman/Islamic history, and prestigious scholarly commentary on 'recovered' texts.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, translation_and_transmission_infrastructure, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(orthographic_legitimacy_kernel__continuity_reading, translation_and_transmission_infrastructure).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, religious_scholars).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity with religious and cultural tradition by preserving the script that carries centuries of accumulated interpretation, legal precedent, and spiritual authority. A single script unifies the reading community across time — the Quran, hadith, classical jurisprudence, and historical narrative all speak in one linguistic form.
% TRANSFER_FUNCTION: Transfers interpretive authority and access control from the general reading public to specialized institutions (religious scholars, text custodians, academic specialists). Those who learn the old script become gatekeepers; those who cannot read it must request mediation. The constraint moves legitimacy from democratized literacy to curated, institutionalized transmission.
% ABSENT_VOICES: Post-reform generations who might object to their structural exclusion from pre-reform texts are themselves constituted by the reform — they have no native voice in the script question because they are the ones severed from it. Their potential objection (that they should have unmediated access to the cultural record) is structurally unrepresentable in a system where that access is framed as optional, elite, or supernumerary to modern citizenship.
% DISAPPEARANCE_RATIONALE: If the constraint (script incompatibility as a barrier to historical access) vanished — if post-reform generations could read pre-reform texts directly — the entire institutional apparatus of translation, scholarly mediation, and religious gatekeeping would reorganize. Direct access would dissolve the monopoly on interpretation. Libraries and religious institutions would face pressure to make originals available; scholarly prestige would attach to direct engagement rather than mediated commentary. The power structure of religious authority would shift.
% FOUNDING_PROBLEM: After orthographic reform, the new script became the administrative, educational, and commercial standard; the old script became inaccessible to the newly literate. This severed post-reform readers from centuries of cultural, legal, and spiritual texts written in the old script. The founding problem is: how do we preserve the cultural record and religious continuity when the new generation cannot read the old form?
% FOUNDING_PROBLEM_CORROBORATION: Post-reform cohorts report genuine inability to read pre-reform religious texts without additional training; historians attest that major archives remain inaccessible to the general public; religious authorities assert continuity requires script preservation. Independent scholars outside the benefiting institutions document the access gap and its consequences for cultural transmission. The founding problem is attested independently; no corroboration comes solely from preservationists.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.38–0.41 across the interval) is moderate, not high, because the constraint presents itself as natural (script incompatibility IS a fact of linguistics) rather than as constructed extraction. However, the institutional dynamics reveal extraction: the ability to read old texts is gatekept through curriculum control, and beneficiaries profit from that gatekeeping. Suppression (0.62) is substantial because the constraint is maintained through educational policy (which scripts are taught, where, to whom) and institutional control of text access — not through direct coercion, but through structural design: post-reform generations are never offered the option to learn the old script in public education. Theater (0.28) is moderate-low: there is real cost to preserving script knowledge and textual continuity, but an increasing share of the institutional work is performative — the scholarly apparatus has grown to celebrate 'recovery' of texts, when the real function is gatekeeping. Accessibility collapse (0.79) is high: once you are educated in the new script only, alternatives (learning the old script, accessing unmediated texts) collapse completely — there is no path without investing years in specialized study outside normal education. Resistance (0.71) is substantial: the post-reform generations and modernist forces resist the constraint; religious authorities push back against proposals to make the old script accessible; the institutional tension persists for a century. The measurement series shows moderate upward drift in extractiveness and theater ratio mid-interval (as institutions solidify gatekeeping and scholarly apparatus expands around mediation), then slight decline as some decentralization of knowledge (printing, partial digitization) reduces the institutional monopoly, though the core barrier persists. One shared time grid governs all metrics at every point.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats inhabit incommensurable worlds. From the post-reform cohort's view, the constraint is an extractive barrier maintained by institutional gatekeeping (snare: they cannot exit, cannot read the texts, must depend on specialists). From the religious scholar's view, the constraint is coordination of unbroken tradition — a rope, even a mountain (script is simply what historical texts are written in; preserving access requires preserving the script). The gap is not just in values but in structural relationship: one party is trapped, one party arbitrages access.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-reform generations sit at directionality d ≈ 0.95 (full target): they bear the entire cost of severance with no choice to exit; their identity as modern readers is locked into the new script. Religious scholars sit at d ≈ 0.15 (beneficiary end): they collect interpretive authority and professional prestige without paying the cost of maintaining the constraint (the cost falls on newly literate cohorts). The constraint is a SNARE from the payer seat (post-reform generations), a ROPE from the beneficiary seat (religious scholars as coordinators of unbroken tradition), and a MOUNTAIN from the modernist state architect seat (script incompatibility is a natural fact they imposed, viewed from their analytical distance). The per-seat divergence is the point: the engine computes this from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This story diagnoses a FALSE SUMMIT: the constraint is claimed as mountain (script incompatibility is physical and emerges naturally), but declared beneficiaries exist (religious scholars, text custodians). The FSM signature fires because beneficiaries are present on a mountain claim, triggering investigation. The omega variables document the irreducible ambiguity: Is this a genuine constraint of linguistic structure (a mountain) or a constructed institutional barrier (a snare/tangled rope using linguistic structure as cover)? The metrics support the latter: if this were a pure natural law, there would be no organized beneficiaries, no suppression (no need to enforce what emerges naturally), and no theater. The fact that institutions gatekeep access, educational policy controls which scripts are taught, and specialized professions profit from mediation suggests the constraint is a TANGLED ROPE (genuine coordination function — preserving textual tradition — layered with extraction via gatekeeping) or a SNARE (the gatekeeping is the primary function, tradition-preservation is the justification). The false-summit reading is the point: the natural-law frame ('script incompatibility is just how language works') masks the extraction (institutional control of access). This is exactly the kind of constraint FSM is designed to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_construct,
    'Is script incompatibility a natural fact (mountain) or a constructed institutional barrier deployed to maintain gatekeeping?',
    'Counterfactual: in a society where post-reform generations were offered biliteracy education in both scripts, would access to pre-reform texts normalize, dissolving the gatekeeping function? Or does the incompatibility persist as a physical fact regardless of institutional effort? Natural experiment: examine jurisdictions that maintained both scripts in public education after reform.',
    'If script incompatibility persists even with dual education available, it is a genuine natural law (mountain). If widespread biliteracy dissolves the gatekeeping monopoly, the constraint is a TANGLED ROPE (genuine coordination + extraction via educational suppression) or SNARE (extraction dressed as preservation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_construct, empirical, 'Whether the barrier is linguistic fact or institutional gatekeeping.').

omega_variable(
    continuity_vs_severance_framing,
    'Does preserving the old script in post-reform society serve continuity with tradition, or does it serve to mark tradition as inaccessible and thereby control who speaks for it?',
    'Examine whether institutions that claim to preserve continuity (religious authorities, text custodians) actively work to make pre-reform texts directly readable to the public, or whether they prefer to mediate access and maintain their interpretive monopoly.',
    'If gatekeeping is incidental to genuine preservation efforts, the constraint tilts toward rope. If gatekeeping is the primary institutional function and preservation is the stated justification, the constraint is SNARE.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_severance_framing, conceptual, 'Whether the constraint''s function is tradition-preservation or access-control.').

omega_variable(
    post_reform_generation_agency,
    'Is the post-reform generation''s identity-lock into the new script a structural fact (they were educated only in it, no exit), or a choice sustained by institutional messaging (they could learn the old script if they chose)?',
    'Survey post-reform cohorts: do they experience script incompatibility as an immutable fact of their literacy, or as a remediable gap? Are dual-script educational pathways advertised and accessible, or are they positioned as elite/specialist/optional? Measure the cost and friction of learning the old script post-hoc.',
    'If exit is genuinely trapped (full structural closure), the constraint is more extractive and the payer is more victimized. If identity-lock is partly internalized (they accept the new script as ''theirs'' and the old as ''other''), understanding suppression as partly internalized opens post-exit interventions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_reform_generation_agency, empirical, 'Whether post-reform generations'' script lock is structural or internalized.').

omega_variable(
    religious_authenticity_claim,
    'Is script preservation genuinely necessary for religious authenticity (reading the Quran in the original script is a religious requirement), or is religious authenticity constructed to justify script preservation?',
    'Examine Islamic jurisprudence and theology on translation and script: do authoritative sources require preservation of original script for religious validity, or is this a retrospectively articulated legitimacy claim?',
    'If script is religiously required, the constraint carries genuine coordination (religious practice requires preserved tradition), and the constraint tilts toward ROPE. If authenticity is a constructed frame, the constraint is SNARE (gatekeeping dressed as religious requirement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authenticity_claim, conceptual, 'Whether script preservation is a religious requirement or a constructed justification.').

omega_variable(
    false_summit_ambiguity,
    'Is orthographic incompatibility a natural law that happens to have beneficiaries (false summit: natural claim with identifiable rent-collectors), or is it an extractive institutional arrangement using linguistic structure as cover?',
    'FSM signature: monitor whether the measured suppression (0.62) and named beneficiaries (religious scholars, text custodians) persist even in jurisdictions that attempt to normalize biliteracy. If suppression remains high and beneficiaries maintain gatekeeping despite biliteracy availability, the natural-law frame is false — the extraction survives the removal of the linguistic barrier, indicating the constraint is constructed, not emergent.',
    'Engine classification: if FSM fires and the override triggers, the constraint reclassifies from MOUNTAIN to TANGLED_ROPE or SNARE. The false summit is detected; the constructed extraction is exposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_ambiguity, empirical, 'FSM candidate: Mountain with beneficiaries — is the natural-law claim itself a cover story?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(orth_tr_t12, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(orth_tr_t50, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(orth_tr_t75, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 75, 0.29).
narrative_ontology:measurement(orth_tr_t100, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(orth_be_t12, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(orth_be_t50, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(orth_be_t75, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 75, 0.39).
narrative_ontology:measurement(orth_be_t100, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(orth_su_t12, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(orth_su_t50, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement(orth_su_t75, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement(orth_su_t100, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__continuity_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the orthographic_legitimacy_kernel. The kernel is contested: what makes a script legitimate? The continuity reading claims legitimacy derives from preserving access to historical, religious, and literary tradition. Sibling readings (instrumentalist and modernist) make different claims about the same standing arrangement (post-reform orthographic system). Each reading authors a different ε, different beneficiary/victim structure, and different type because the readings assess the arrangement by different criteria. This story is specifically the continuity reading and is linked via network.affects_constraints to the other readings. All three readings share the same constraint landscape (same state, same parties) but differ on what legitimacy criterion they apply. The three stories together form the constraint family for the orthographic legitimacy dispute. Do not merge the three readings into one story with a 'measurement basis' parameter — each reading is a separate constraint with its own ε-invariance and its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
