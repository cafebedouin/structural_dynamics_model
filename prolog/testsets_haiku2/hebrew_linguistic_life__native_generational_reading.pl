% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__native_generational_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Native Generational Linguistic Life Standard
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   The native-generational reading of linguistic life asserts that a
 *   language is alive only when children acquire it as mother tongue and use
 *   it for all daily secular functions. This reading emerged during the
 *   Hebrew revival (late 19th–20th centuries) as the measure by which
 *   Hebrew's status shifted from liturgical-only dormancy to living
 *   vernacular. The reading's structural consequence is the coercive
 *   displacement of existing generationally-transmitted mother tongues —
 *   Yiddish, Ladino, Judeo-Arabic — from child-rearing contexts. The
 *   constraint operates as enforced standardization: an institutional agenda
 *   favoring Hebrew-only acquisition and use as the sole criterion of
 *   linguistic life. Yiddish and Ladino speakers carried already-living,
 *   already-generational languages; the native-generational standard
 *   redefines their linguistic life as insufficient or dead because it is not
 *   Hebrew. This reading is one instantiation of the contested kernel
 *   'hebrew_linguistic_life'; sibling readings include the
 *   liturgical-preservation reading (Hebrew was always alive through
 *   continuous sacred-text transmission) and the marketplace-pidgin reading
 *   (Hebrew's revival functioned as inter-communal coordination tool
 *   regardless of native-speaker status). The claim/metric gap is
 *   intentional: the constraint is CLAIMED as tangled_rope (coordination +
 *   enforcement) while extraction measurements reflect substantial asymmetry.
 *   The engine computes the per-seat divergence from the structural data.
 *
 * KEY AGENTS:
 *   - hebrew_revival_institutions — agenda-setter (institutional power); sets native-generational standard; collects cultural prestige from 'revival' frame
 *   - yiddish_speakers — payer (organized power, identity-locked exit); bear displacement costs; exist in resistance to the constraint
 *   - ladino_speakers — payer (organized power, identity-locked exit); carry living mother-tongue languages treated as linguistically dead under the native-generational reading
 *   - hebrew_language_planners — agenda-setter + beneficiary (institutional power); theorize and operationalize the standard; derive professional authority
 *   - jewish_children_in_yishuv — beneficiary + payer (powerless, trapped); acquire Hebrew; lose intergenerational transmission of parent languages
 *   - israeli_state — agenda-setter (institutional power, national scope); inherits and enforces the standard through law and policy
 *   - linguistic-plurality advocates — excluded (moderate power, constrained exit); would object to Hebrew-only but are kept outside the policy conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.82).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.78).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Native Generational Linguistic Life Standard").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '2a2ee890-01a5-493c-9067-b843817861ed').
narrative_ontology:cs_kernel_codification('2a2ee890-01a5-493c-9067-b843817861ed', formalized).
narrative_ontology:cs_authority_grounding('2a2ee890-01a5-493c-9067-b843817861ed', extraction).
narrative_ontology:cs_interpretation_layer_present('2a2ee890-01a5-493c-9067-b843817861ed').
narrative_ontology:cs_reading_relation('2a2ee890-01a5-493c-9067-b843817861ed', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('2a2ee890-01a5-493c-9067-b843817861ed', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('2a2ee890-01a5-493c-9067-b843817861ed', foundational, generational_transmission_requirement).
narrative_ontology:cs_axiom_status(generational_transmission_requirement, holdable).
narrative_ontology:cs_axiom_grounding('2a2ee890-01a5-493c-9067-b843817861ed', generational_transmission_requirement, deontological).
narrative_ontology:cs_axiom('2a2ee890-01a5-493c-9067-b843817861ed', foundational, secular_mundane_sufficiency).
narrative_ontology:cs_axiom_status(secular_mundane_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('2a2ee890-01a5-493c-9067-b843817861ed', secular_mundane_sufficiency, deontological).
narrative_ontology:cs_reference_frame('2a2ee890-01a5-493c-9067-b843817861ed', hebrew_native_generational_linguistic_life).
narrative_ontology:cs_drift_state('2a2ee890-01a5-493c-9067-b843817861ed', contemporary_post_1980s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a2ee890-01a5-493c-9067-b843817861ed', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revival_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, ashkenazi_jewish_nationalist_movements).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_language_planners).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, jewish_children_in_yishuv).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, jewish_children_in_yishuv).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the native-generational standard as the measure of linguistic life. Operates schools, publishes curricula, administers language-of-instruction policy in Yishuv settlements and later Israeli state. Defines Hebrew-only secular speech as the legitimacy criterion. Collects institutional authority and cultural prestige from the claim that they are enabling genuine linguistic revival rather than displacement.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revival_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Carry a living, generationally-transmitted language with secular, sacred, and intimate functions already intact. Face institutional pressure (school policy, settlement culture, nationalist ideology) to abandon Yiddish in favor of Hebrew acquisition as the child's mother tongue. Exit involves severing cultural identity, family transmission, and community belonging — the languages are not interchangeable utility choices but identity-constitutive. Yiddish children in Hebrew-only schools experience the linguistic shift as cultural erasure.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    organized, biographical, identity_locked, global).

% Sephardic and Mizrahi communities carrying Ladino (Judeo-Spanish) and Judeo-Arabic as living vernaculars with generational depth and secular/sacred function. Encounter the same institutional pressure to abandon mother-tongue transmission in favor of Hebrew-only child-rearing. The constraint treats their existing linguistic life as insufficient or false because it is not Hebrew, despite meeting the native-generational criterion for their own languages.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    organized, biographical, identity_locked, regional).

% Mizrahi and Yemenite Jews with Arabic as inherited mother tongue and secular vernacular. Face pressure to abandon Arabic acquisition and replace it with Hebrew in child-rearing, despite Arabic meeting the native-generational criterion. The constraint redefines linguistic life to exclude Arabic-Jewish linguistic space, treating Hebrew-only as the sole legitimate measure.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jews, payer,
    moderate, biographical, constrained, regional).

% Linguists, educators, and nationalist intellectuals who theorize the native-generational standard and operationalize it in policy. They benefit from professional authority, publication, and institutional position as the architects of linguistic revival. They present the constraint as rescuing a dead language, eliding the fact that it operates through displacement of living languages.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_language_planners, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, hebrew_language_planners, beneficiary).

% Acquire Hebrew as mother tongue through institutional schooling, gaining access to nationalist institutions, peer belonging, and modern secular identity. They also lose transmission of parent/grandparent languages and carry the constraint's enforced linguistic standardization as the naturalness of childhood. They cannot refuse or exit: schooling is compulsory and family language shift is mediated by institutional pressure on parents.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, jewish_children_in_yishuv, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, jewish_children_in_yishuv, payer).

% Historians and linguists external to the revival movements, documenting Hebrew's actual status 70-1880 CE (liturgical-only, dormant as living vernacular) and assessing the claim that revival required the native-generational standard versus other coordination possibilities (liturgical preservation, pidgin coordination, multilingual coexistence).
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, pre_1880_hebrew_scholars, observer,
    analytical, civilizational, analytical, global).

% Communities and theorists arguing that multilingual child-rearing, maintenance of multiple mother tongues, and plural linguistic ecologies are possible and desirable. They are excluded from the policy conversation because the native-generational standard presents Hebrew-only as the sole measure of linguistic life; plurality is treated as linguistic death or degradation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_plurality_advocates, excluded,
    moderate, generational, constrained, global).

% Inherits and institutionalizes the native-generational standard as state language policy. Enforces it through compulsory Hebrew-only education, policy against minority-language instruction, and nationalist ideology equating linguistic life with Hebrew acquisition. Derives state coherence and national identity from the claim that linguistic standardization is linguistic revival.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, hebrew_revival_institutions).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared vernacular for children and families across dispersed Jewish communities with different inherited languages (Yiddish, Ladino, Judeo-Arabic). Enables intergenerational transmission of a revived language in secular, quotidian contexts after centuries of liturgical-only existence. Solves the coordination problem of creating a living mother tongue for a dispersed people.
% TRANSFER_FUNCTION: Moves linguistic authority, child-rearing practice, and intergenerational transmission from existing mother-tongue languages (Yiddish, Ladino, Judeo-Arabic) to Hebrew-only. Transfers cultural prestige and institutional legitimacy to the native-generational Hebrew standard. Transfers linguistic displacement costs (language loss, identity rupture, cultural erasure) to speaker communities of non-Hebrew languages.
% ABSENT_VOICES: Yiddish, Ladino, and Judeo-Arabic speaker communities are structurally pressured but not primary agenda-setters; their objection — that they already have generational mother tongues meeting the criterion — is treated as parochial or insufficient because those languages are not Hebrew. Communities advocating plural multilingualism are excluded from the policy conversation entirely; their claim that children can acquire multiple mother tongues and maintain linguistic diversity would dissolve the constraint's framing.
% DISAPPEARANCE_RATIONALE: If the native-generational standard and its enforcement vanished, Jewish communities would maintain or recover Yiddish, Ladino, and Arabic transmission to children; multilingual child-rearing would resume; the linguistic landscape would reorganize around plural mother tongues rather than Hebrew-only standardization. The constraint exists because without active institutional pressure and ideology supporting it, existing living languages would persist as primary transmission vehicles.
% FOUNDING_PROBLEM: Hebrew was dormant as a living vernacular 70-1880 CE, preserved in liturgy and textual study but not acquired as mother tongue or used in everyday secular speech by children. Late 19th-century Yishuv faced the coordination problem of creating a shared secular language across diaspora communities with different vernaculars (Yiddish, Ladino, Arabic-Jewish). The founding problem claims revival required native-generational acquisition as the sole measure of linguistic life.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew-revival institutions and Israeli state attest the founding problem is still live — they present continuous institutional effort as necessary to maintain Hebrew-only native acquisition. External historians and linguists attest that the founding problem (creating a shared secular language) was solved by 1980-2000 CE and the constraint now persists as identity maintenance and enforced standardization, not coordination necessity. Academic linguists and multilingual societies demonstrate that alternative coordination mechanisms existed (pidgin use, multilingual coexistence, mixed-language children) and remain viable. The constraint persists against evidence that its founding problem is resolved.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__native_generational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__native_generational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) and rises over the interval because the constraint operates asymmetrically: Hebrew-revival institutions benefit from linguistic standardization while non-Hebrew speakers bear the costs of language displacement. The constraint is called tangled_rope because it solves a real coordination problem (creating a shared secular language for dispersed communities) while simultaneously extracting through the displacement of existing living languages. Suppression is high (0.78) because maintaining Hebrew-only requires active institutional pressure against the continued use and transmission of Yiddish, Ladino, and Arabic-Jewish. Suppression rises sharply 0→60 as institutional infrastructure scales and then stabilizes as the norm becomes internalized; the measurement tracks both active policy enforcement and internalized linguistic identity. Theater is moderate (0.31): the constraint performs revival — celebrating Hebrew's resurrection as a living language — while the functional necessity of Hebrew-only (versus multilingual coordination) diminishes over time. By the late 20th century, the theater ratio reflects enforcement of identity through enforced standardization rather than solving coordination problems. Accessibility-collapse is moderate-high (0.71) because once the native-generational standard is institutionalized, alternatives (plural multilingualism, Yiddish maintenance, Arabic-Jewish preservation) appear increasingly illegitimate or impossible, not because they are technically unavailable but because the constraint has redefined linguistic life. Resistance is moderate (0.64): non-Hebrew speaker communities resist the standard through family-language maintenance and cultural-preservation movements, but their resistance is structural — pressed against the full weight of state and institutional machinery — and has limited effect on policy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Hebrew revival institutions, Israeli state) experiences the constraint as enabling — they have revived a dead language and created a living vernacular where none existed. From this seat, the native-generational standard is the necessary and sufficient definition of linguistic life. The payer seats (Yiddish, Ladino, Judeo-Arabic speakers) experience the constraint as coercive — they already had living, generational mother tongues and the constraint redefined their linguistic life as insufficient or dead. From these seats, the native-generational standard is enforced displacement dressed as revival. The engine computes these divergent readings from the structural data: the payer seats are identity-locked (cannot exit without cultural rupture), have high d (directionality toward target), and experience extraction; the agenda-setter seats have arbitrage exit options (can shift language policy), low d (beneficiary position), and collect institutional authority. The gap is not a measurement error — it is the structural fact the constraint instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew-revival institutions and the Israeli state derive institutional authority, prestige, policy control, and cultural narrative from the native-generational standard. Their d-value sits near 0.0 (beneficiary). Yiddish and Ladino speakers experience displacement of existing mother-tongue transmission, cultural stigmatization, institutional pressure on family language choices, and identity rupture (if they comply with the standard). Their d-value sits near 1.0 (full target). Jewish children in the Yishuv occupy the dual-role zone: they benefit from access to institutions and peer belonging through Hebrew acquisition, but they bear the cost of severed intergenerational transmission of parent/grandparent languages. Their d-value sits near 0.5 (symmetric, with slight negative d due to powerlessness and trapped exit). Hebrew-language planners (institutional power, arbitrage exit) sit near 0.0–0.1 (beneficiary through professional authority). No directionality override is needed: the structural derivation from beneficiary/victim + power + exit captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a shared secular language for dispersed communities with different vernaculars) was genuinely live 1880–1950 CE. The native-generational standard provided one solution path among several: multilingual coordination, pidgin coordination for commerce, and liturgical-only preservation were alternative coordination mechanisms that would not have required mother-tongue displacement. By 1980–2000 CE, Hebrew was fully established as a living generational language; the founding problem was solved. Yet the constraint persists and the suppression requirement remains high. This is the mandatrophy signature: the constraint continues beyond its functional necessity. The contemporary native-generational standard operates not because coordination still requires it, but because enforced linguistic standardization has become a mechanism for state identity maintenance and cultural prestige. The theater-ratio rise from 0.10 (early revival, genuine coordination) to 0.31 (contemporary, performative enforcement) tracks this drift from coordination to identity performance. The resistance from excluded voices (linguistic-plurality advocates, minority-language communities) is not an indicator that the founding problem persists — it is an indicator that the constraint has shifted from coordination to enforcement. Mandatrophy is not resolved; it is the ongoing condition of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_linguistic_life_criterion,
    'Is the native-generational criterion (mother-tongue acquisition + all daily secular functions) an objective measure of linguistic life, or a constructed standard that benefits Hebrew-revival institutions by redefining competing languages as dead?',
    'Examine whether non-Hebrew languages (Yiddish, Ladino, Judeo-Arabic) meet the criterion in their own right. If they do, and were treated as ''dead'' only after institutional pressure, the criterion is revealed as constructed, not objective.',
    'If constructed, the constraint reclassifies as pure extraction disguised as coordination. If objective, the constraint remains tangled_rope with genuine coordination function. The empirical answer determines whether this reading is a legitimate linguistic classification or a nationalist ideology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_of_linguistic_life_criterion, empirical, 'Whether the native-generational criterion is objective or constructed.').

omega_variable(
    alternative_coordination_viability,
    'Could the Jewish diaspora communities have achieved secular-language coordination through multilingual coexistence, pidgin formation, or plurality without requiring Hebrew-only mother-tongue displacement?',
    'Counterfactual historical analysis: examine multilingual societies that maintained plural mother-tongue transmission while achieving inter-community coordination (Switzerland, Singapore, Canada post-1970s). Assess whether such models were technically available to the Yishuv/Israeli context.',
    'If multilingual coordination was technically viable, the native-generational standard''s enforcement is revealed as a policy choice serving identity, not a necessity imposed by the coordination problem. If multilingual coordination was not viable in the historical context, the extraction is justified as a cost of necessary standardization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_viability, conceptual, 'Whether Hebrew-only standardization was coordinatively necessary or chosen for identity reasons.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression primarily structural (institutional policy, school mandates, state law) or internalized (linguistic identity, speaker shame, internalized linguistic hierarchy)?',
    'Post-policy-change analysis: if language-preservation policies are enacted (legal protection for minority-language education, state media in Arabic-Jewish, Yiddish curricula), does suppression drop sharply (structural was dominant) or persist (internalized was dominant)?',
    'If mostly structural, remedies are policy-level and rapid. If mostly internalized, the constraint carries into speaker identity even after policy change; exit becomes more difficult and remedies are slower (generational re-education, cultural reclamation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized components of measured suppression.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Do the native-generational and liturgical-preservation readings logically foreclose each other in a single institutional framework, or do they coexist as live contested positions across different parties?',
    'Examine whether any institutional actor or policy document holds both readings simultaneously (e.g., Hebrew is alive through native-generation acquisition AND through unbroken sacred-text transmission). If none does, they coexist; if contradiction is explicit, they foreclose.',
    'If they foreclose, the native-generational reading''s dominance is not contingent — it is architecturally required in any single framework. If they coexist, the native-generational reading''s institutional dominance is a policy choice, not a logical necessity, and the constraint is subject to reframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether sibling readings logically foreclose or contingently coexist.').

omega_variable(
    kernel_contestation_site,
    'Where is the actual disagreement between readings located: in the definition of ''alive'' (empirical — what state does a language need to be in), or in the values assigned to different linguistic statuses (normative — how much do we care about native-generation transmission versus liturgical preservation)?',
    'Separate empirical from normative disagreement in policy debates. If disagreement is empirical, it is resolvable by evidence. If normative, it is a value difference that no evidence alone resolves.',
    'If empirical, the contest is whether Hebrew meets the criterion; parties should agree on adjudication once evidence is clear. If normative, the readings reflect irreducible value disagreements about what linguistic life should prioritize; the constraint''s persistence depends on institutional power, not empirical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_site, conceptual, 'Whether kernel disagreement is empirical or normative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__native_generational_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__native_generational_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__native_generational_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__native_generational_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__native_generational_reading, theater_ratio, 100, 0.31).
narrative_ontology:measurement(hebr_tr_t140, hebrew_linguistic_life__native_generational_reading, theater_ratio, 140, 0.31).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(hebr_be_t140, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 140, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 100, 0.76).
narrative_ontology:measurement(hebr_su_t140, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 140, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% The constraint hebrew_linguistic_life is a contested kernel with three structurally distinct readings. Each reading instantiates a different constraint with different extraction profiles and beneficiary/victim structures. The native_generational_reading (this file) treats linguistic life as requiring native-generation mother-tongue acquisition + secular speech, redefining non-Hebrew languages as linguistically dead and displacing them institutionally. The liturgical_preservation_reading treats linguistic life as continuous sacred-text transmission regardless of vernacular use, leaving plural linguistic spaces intact. The marketplace_pidgin_reading treats linguistic life as inter-communal functional coordination regardless of native-speaker status, enabling multilingual coexistence. All three readings share the same kernel (what constitutes linguistic life) but produce incompatible constraint structures. Network edges link the readings to enable contamination propagation analysis: the native-generational reading influences both sibling readings by dominating state policy and redefining linguistic legitimacy; it forecloses them in institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
