% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia Universal Heritage Reading
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates the universal heritage reading of the Hagia
 *   Sophia substrate kernel. Under this reading, the site's legitimacy
 *   derives from its status as shared human cultural heritage that transcends
 *   any single religious or national claim. The reading was enforced through
 *   a technocratic museum administration operating under a secular
 *   constitutional framework, particularly from 1934 onward. It coordinates
 *   global access, research, and tourism while simultaneously extracting
 *   revenue and ideological legitimacy from the suppression of Islamic
 *   worship claims and the exclusion of Orthodox restitution demands.
 *
 * KEY AGENTS:
 *   - museum_administration: Agenda-setter (institutional/constrained) â technocratic steward administering the site under secular constitutional authority
 *   - tourism_sector: Primary beneficiary (organized/mobile) â captures revenue from heritage commodification
 *   - heritage_scholarship_sector: Secondary beneficiary (organized/mobile) â gains access and prestige from universal museum status
 *   - secularist_turkish_elites: Ideological beneficiary (powerful/constrained) â capture signal of secular modernity
 *   - islamic_worship_claimants: Primary payer/target (organized/identity_locked) â bear suppressed worship rights and religious exclusion
 *   - orthodox_restitution_advocates: Excluded voice (organized/constrained) â structurally absent from sovereignty negotiations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.74).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.85).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia Universal Heritage Reading").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, 'cb43d3f9-a44e-4339-b106-3edd7195d781').
narrative_ontology:cs_kernel_codification('cb43d3f9-a44e-4339-b106-3edd7195d781', formalized).
narrative_ontology:cs_authority_grounding('cb43d3f9-a44e-4339-b106-3edd7195d781', extraction).
narrative_ontology:cs_interpretation_layer_present('cb43d3f9-a44e-4339-b106-3edd7195d781').
narrative_ontology:cs_reading_relation('cb43d3f9-a44e-4339-b106-3edd7195d781', hagia_sophia_substrate__islamic_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('cb43d3f9-a44e-4339-b106-3edd7195d781', hagia_sophia_substrate__orthodox_restitution_reading, influences).
narrative_ontology:cs_axiom('cb43d3f9-a44e-4339-b106-3edd7195d781', foundational, heritage_transcends_worship_claims).
narrative_ontology:cs_axiom_status(heritage_transcends_worship_claims, holdable).
narrative_ontology:cs_axiom_grounding('cb43d3f9-a44e-4339-b106-3edd7195d781', heritage_transcends_worship_claims, conventional).
narrative_ontology:cs_axiom('cb43d3f9-a44e-4339-b106-3edd7195d781', foundational, secular_administration_as_neutral_steward).
narrative_ontology:cs_axiom_status(secular_administration_as_neutral_steward, holdable).
narrative_ontology:cs_axiom_grounding('cb43d3f9-a44e-4339-b106-3edd7195d781', secular_administration_as_neutral_steward, instrumental).
narrative_ontology:cs_reference_frame('cb43d3f9-a44e-4339-b106-3edd7195d781', secular_universal_heritage_framework).
narrative_ontology:cs_drift_state('cb43d3f9-a44e-4339-b106-3edd7195d781', contemporary_post_2020_reversion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cb43d3f9-a44e-4339-b106-3edd7195d781', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_claimants).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, secular_constitutional_supremacy).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, unesco_universal_heritage_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the site under the secular constitutional framework and international heritage protocols. Controls entry, exhibition, and worship permissions. Staffed by technocratic heritage professionals who frame decisions as neutral preservation. Cannot unilaterally alter the site's legal status but enforces the daily operational regime.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, museum_administration, agenda_setter,
    institutional, generational, constrained, national).

% Captures revenue from ticketed entry, guided tours, hospitality, and merchandise tied to the site's global brand. Benefits from the museum classification that permits unrestricted visitor access and secular marketing. Could pivot to other heritage sites but profits are concentrated here due to the site's unique status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Gains research access, funding, and disciplinary prestige from the site's classification as neutral universal heritage. Produces scholarship under the frame of shared human patrimony. Exit to other Byzantine or Ottoman sites is possible but Hagia Sophia remains the anchor credential.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_sector, beneficiary,
    organized, generational, mobile, global).

% Capture ideological signaling from the site's secular museum status: evidence of Kemalist modernity, constitutional supremacy over religious authority, and Western-facing cosmopolitanism. Their political identity is partially constituted by this symbolic arrangement. Exit would mean accepting Islamic sovereignty over a core republican monument.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    powerful, generational, constrained, national).

% Bear the suppression of full communal prayer rights inside a site they regard as a mosque by legitimate endowment. Religious identity fuses with the specific structure; alternative mosques do not substitute the symbolic and spiritual claim. Subject to state enforcement of museum hours, ticketed entry, and exhibition protocols during periods of this reading's dominance.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_claimants, payer,
    organized, civilizational, identity_locked, national).

% Assert founding ecclesiastical identity and seek restitution or shared ecclesiastical control. Structurally excluded from sovereignty negotiations between the Turkish state and UNESCO; their claims are filed under minority-rights discourse and ignored in the universal-heritage framework. No institutional seat at the table where the site's status is administered.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_advocates, excluded,
    organized, civilizational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents destructive sectarian conflict over a singular contested sacred site by suspending exclusive religious ownership under an internationalized, technocratic heritage regime that facilitates global access, research, and tourism.
% TRANSFER_FUNCTION: Moves revenue and symbolic legitimacy from religious worship claimantsâprimarily Islamic and secondarily Orthodoxâto the secular state apparatus, tourism operators, and international heritage institutions. Moves control over spatial use and ritual access from religious authorities to museum administrators.
% ABSENT_VOICES: Islamic worship claimants seeking unrestricted prayer rights are partially present in public discourse but excluded from institutional decision-making. Orthodox restitution advocates are absent from sovereignty negotiations. Local communities whose religious geography is overwritten by UNESCO tourism management are absent.
% DISAPPEARANCE_RATIONALE: If the universal heritage frame disappeared overnight, tourism flows would reorganize around sectarian pilgrimage or collapse, the secular constitutional order would lose a symbolic anchor, and religious sovereignty contestation would return to the foreground. The site's daily use, revenue streams, and international legal status would all shift.
% FOUNDING_PROBLEM: The risk of interstate and inter-religious violence over exclusive control of a monument claimed by multiple civilizations, and the absence of a neutral stewardship mechanism under the collapsing Ottoman theocratic order.
% FOUNDING_PROBLEM_CORROBORATION: International heritage organizations and secular constitutional historians attest to the conflict-prevention rationale. Islamic religious authorities and Orthodox patriarchates attest that the 'neutralization' was expropriation masquerading as peacekeeping, and that the true problem was sovereignty, not sectarianism. Independent historians are divided.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the universal heritage frame channels tourism revenue and secular-modernist ideological signaling while suppressing competing religious claims. Suppression (0.85) is higher still because the reading's persistence depends on active state enforcement of museum rules against worship claims and restitution demands. Theater ratio (0.55) reflects the increasing performative quality of 'universalism' as actual practice drifted toward hybrid mosque-museum status and the frame became more about diplomatic signaling than neutral stewardship. Accessibility collapse (0.70) is substantial because alternatives (religious sovereignty, worship free access) are legally barred and culturally marginalized, though they persist in discourse. Resistance (0.72) is high because both Islamic and Orthodox claimants mount sustained symbolic, legal, and political opposition.
 *
 * PERSPECTIVAL GAP:
 *   The secularist-tourism seat experiences the constraint as genuine coordination: a necessary neutralization preventing sectarian violence and enabling global access. The Islamic worship seat experiences the same structure as active extraction and religious dispossession. The Orthodox seat experiences exclusion from the negotiating table entirely. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The museum administration, tourism sector, and heritage scholarship sector sit near the beneficiary end: they are subsidized by the constraint's operation (funding, revenue, prestige, institutional purpose). Secularist Turkish elites are ideological beneficiaries with constrained exit because their political identity is fused with the arrangement. Islamic worship claimants sit near the full-target end: they bear the extraction of suppressed worship rights and spatial dispossession, with identity-locked exit because Hagia Sophia's religious significance is non-substitutable. Orthodox restitution advocates are excluded rather than coordinated; their exclusion is a structural precondition for the reading's coherence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the arrangement as pure extraction (snare) because there is a genuine coordination function: the site is physically preserved, global scholarship proceeds, and sectarian violence has been averted for decades. It prevents mislabeling as pure coordination (rope) because the same structure that preserves also actively suppresses legitimate worship claims and channels revenue asymmetrically. The extraction and coordination are inextricable: you cannot have the universal heritage museum without the ticketed entry, the security perimeter, and the legal suppression of exclusive religious claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the universal heritage reading a genuinely neutral coordination framework, or does it covertly embed secular-modernist extraction that suppresses non-secular claims?',
    'Comparative analysis of access patterns, revenue flows, and worship rights under the sibling readings; cross-site comparison of contested sacred heritage under secular vs. religious administration.',
    'If the reading is structurally inseparable from secular-nationalist extraction, reclassification toward snare; if genuinely neutral, classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the universal heritage frame is neutral or extractive at core.').

omega_variable(
    extraction_accumulation,
    'Did the extractive dimension of the universal heritage reading accumulate over time through tourism commodification, or was it present at the founding secularization in 1934?',
    'Historical economic analysis of tourism revenue, state expenditure on preservation, and worship restriction enforcement across the interval.',
    'If extraction accumulated, the constraint began closer to rope or scaffold and drifted into tangled_rope; if present at inception, it was always tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_accumulation, empirical, 'Temporal origin of the extractive function.').

omega_variable(
    heritage_neutrality_axiom,
    'Can a technocratic secular administration ever function as a neutral steward of a contested sacred site, or does it inherently embed the secularist suppression of religious voice?',
    'Cross-site comparative study of contested religious heritage administered under secular, religious, and hybrid regimes, measuring relative access and conflict outcomes.',
    'If neutrality is structurally impossible, the coordination story is cover and the constraint leans snare; if possible, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(heritage_neutrality_axiom, conceptual, 'Whether secular heritage administration can be genuinely neutral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_univ_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hagia_univ_tr_t10, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(hagia_univ_tr_t20, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(hagia_univ_tr_t30, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(hagia_univ_tr_t40, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(hagia_univ_tr_t50, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(hagia_univ_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hagia_univ_be_t10, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(hagia_univ_be_t20, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(hagia_univ_be_t30, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(hagia_univ_be_t40, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(hagia_univ_be_t50, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 50, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(hagia_univ_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(hagia_univ_su_t10, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(hagia_univ_su_t20, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(hagia_univ_su_t30, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(hagia_univ_su_t40, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(hagia_univ_su_t50, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The Hagia Sophia substrate decomposes into three structurally distinct constraints (readings) with mutually exclusive beneficiary/victim structures and epsilon profiles. This file instantiates the universal heritage reading only; siblings are separate constraints linked via the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
