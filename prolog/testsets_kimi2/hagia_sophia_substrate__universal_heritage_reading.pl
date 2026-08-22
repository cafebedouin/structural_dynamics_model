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
 *   domain: cultural/political/religious
 *
 * SUMMARY:
 *   The Hagia Sophia substrate under the universal heritage reading is the
 *   legal-administrative arrangement that designates the site as a secular
 *   museum and shared human cultural heritage, transcending any exclusive
 *   religious or national claim. Instantiating this reading, the Turkish
 *   secular state (from 1934 onward) deployed technocratic museum
 *   administration to enforce a worship ban, generating high extractiveness
 *   by channeling tourism revenue, scholarly prestige, and ideological
 *   legitimacy toward secularist elites and the global heritage industry,
 *   while actively suppressing Islamic worship claims and foreclosing
 *   Orthodox restitution. The constraint presents itself as neutral
 *   coordination (preservation, global access) but operates as asymmetric
 *   extraction through enforced exclusion of religious use.
 *
 * KEY AGENTS:
 *   - secular_museum_administration: Agenda-setter (institutional/national/constrained) â enforces museum regime and worship ban
 *   - heritage_tourism_industry: Primary beneficiary (organized/global/mobile) â captures visitation revenue
 *   - secularist_turkish_elites: Ideological beneficiary (powerful/national/mobile) â gains symbolic capital from secular modernity
 *   - turkish_muslim_worshippers: Primary payer (powerless/local/identity_locked) â bears suppression of worship claims
 *   - islamic_waqf_claimants: Secondary payer (moderate/national/constrained) â deprived of endowment rights
 *   - orthodox_patriarchate: Excluded voice (institutional/regional/constrained) â restitution claims foreclosed
 *   - unesco_heritage_apparatus: Observer (institutional/global/analytical) â monitors and legitimates heritage frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.78).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.82).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia Universal Heritage Reading").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural/political/religious").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, 'a9e41ec9-3407-4dd0-9d80-e272b4f79435').
narrative_ontology:cs_kernel_codification('a9e41ec9-3407-4dd0-9d80-e272b4f79435', formalized).
narrative_ontology:cs_authority_grounding('a9e41ec9-3407-4dd0-9d80-e272b4f79435', extraction).
narrative_ontology:cs_interpretation_layer_present('a9e41ec9-3407-4dd0-9d80-e272b4f79435').
narrative_ontology:cs_reading_relation('a9e41ec9-3407-4dd0-9d80-e272b4f79435', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('a9e41ec9-3407-4dd0-9d80-e272b4f79435', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('a9e41ec9-3407-4dd0-9d80-e272b4f79435', foundational, universal_patrimony_over_particularism).
narrative_ontology:cs_axiom_status(universal_patrimony_over_particularism, holdable).
narrative_ontology:cs_axiom_grounding('a9e41ec9-3407-4dd0-9d80-e272b4f79435', universal_patrimony_over_particularism, conventional).
narrative_ontology:cs_reference_frame('a9e41ec9-3407-4dd0-9d80-e272b4f79435', secular_universal_heritage_neutrality).
narrative_ontology:cs_drift_state('a9e41ec9-3407-4dd0-9d80-e272b4f79435', contemporary_reconversion_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('a9e41ec9-3407-4dd0-9d80-e272b4f79435', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, heritage_tourism_industry).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_scholars).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, turkish_muslim_worshippers).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_waqf_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the site as a secular museum under the Turkish constitutional framework; enforces the prohibition on religious worship, controls access, ticketing, and curatorial narrative; its authority depends on maintaining the fixed legal status of the building as non-denominational heritage.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secular_museum_administration, agenda_setter,
    institutional, generational, constrained, national).

% Derives substantial revenue from packaged tourism, ticketing, and pilgrimage-adjacent visitation to a globally branded heritage monument; benefits from the worship ban because it permits predictable visitor flows and secular curation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, heritage_tourism_industry, beneficiary,
    organized, biographical, mobile, global).

% Draw ideological legitimacy from the museum status as a symbol of Kemalist secular modernity and Turkish Western-facing identity; the site's universal heritage framing signals that the Turkish state transcends Islamic particularism.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    powerful, generational, mobile, national).

% Benefit from stable, non-sectarian access to the building for architectural research, conservation study, and academic tourism; the museum regime produces an internationally legible heritage discourse and archival infrastructure that privileges scholarly voice.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_scholars, beneficiary,
    organized, biographical, mobile, global).

% Religiously obligated community and local residents for whom the building retains active spiritual significance as a former mosque; legally barred from performing salat inside; their worship claims are structurally suppressed by the heritage enforcement apparatus.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_muslim_worshippers, payer,
    powerless, biographical, identity_locked, local).

% Hold historical waqf endowment claims and administrative rights to the building rooted in Ottoman legal tradition; deprived of property control and religious management by the museum regime; their legal claims are suspended and administratively ignored.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_waqf_claimants, payer,
    moderate, generational, constrained, national).

% Claims ecclesiastical restitution based on the site's founding as a Christian cathedral; its restitution demands and liturgical aspirations are foreclosed by the universal heritage reading, which refuses any single religious assignment including Orthodox control.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_patriarchate, excluded,
    institutional, civilizational, constrained, regional).

% Monitors the site's conservation status and legitimizes the universal heritage framing through World Heritage listing and technical reporting; does not bear costs or collect rents, but its analytical credibility is tied to the durability of the secular museum regime.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_apparatus, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a contested religious structure as nominally neutral shared heritage, enabling global scholarly access, cross-cultural tourism, and non-sectarian conservation without assigning exclusive religious ownership to any single community.
% TRANSFER_FUNCTION: Moves religious use rights away from Muslim and Orthodox claimant communities toward secular state administration; moves tourism revenue from visitors to state and private operators; moves ideological legitimacy toward secular modernist narrative and away from Islamic or Orthodox particularism.
% ABSENT_VOICES: Islamic sovereignty advocates asserting waqf rights and Orthodox restitution claimants seeking ecclesiastical control are structurally excluded from the universal heritage framework; their objections are treated as sectarian particularism rather than legitimate competing claims.
% DISAPPEARANCE_RATIONALE: If the universal heritage constraint vanished, competing religious claims would immediately resurface, tourism and scholarly access would restructure around worship schedules or ecclesiastical control, and the secular ideological signaling embedded in the museum regime would collapse.
% FOUNDING_PROBLEM: Preventing sectarian violent conflict and exclusive appropriation over a monument claimed by multiple religious traditions and national narratives; preserving a unique architectural site from neglect or destructive single-party control.
% FOUNDING_PROBLEM_CORROBORATION: International heritage organizations and secular Turkish jurists corroborate the need for neutral preservation; however, Islamic legal scholars and Orthodox religious authorities outside the benefiting parties attest that the 'neutral' framing is itself an act of exclusion and that the founding problem is better solved through rotational or shared-use arrangements rather than secular monopoly.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the museum regime captures tourism revenue and ideological signaling decoupled from marginal preservation cost. Suppression (0.82) is high because persistence depends on actively policing worship and excluding rival religious claims, not on voluntary coordination. Theater_ratio (0.45) reflects that while genuine preservation occurs, a substantial share of activity performs secular neutrality and modernity. Accessibility_collapse (0.70) captures that once the universal heritage frame dominated, worship and restitution alternatives lost institutional legitimacy. Resistance (0.68) reflects persistent contestation by Islamic and Orthodox claimants. Tangled_rope is structurally warranted: real coordination (preservation, scholarly access) is inextricably bound to asymmetric extraction and active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The secular museum administration and heritage tourism sector experience the constraint as legitimate coordination preserving a world monument. Turkish Muslim worshippers and waqf claimants experience the identical structure as suppression and expropriation â their religious use is the cost of the coordination. The engine computes this divergence from structural data rather than from the self-presentation of any seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (heritage_tourism_industry, secularist_turkish_elites, international_scholars) derive low directionality: the constraint subsidizes their revenue, ideological projects, or scholarly access. Payers (turkish_muslim_worshippers, islamic_waqf_claimants) derive high directionality: the constraint extracts religious use rights and endowment control. The orthodox_patriarchate is excluded rather than directly extracted from in this reading, though its claims are foreclosed. UNESCO sits at an analytical distance with near-zero directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The universal heritage reading risks misclassification as a Rope if one accepts its self-presentation as neutral preservation. However, identifiable victims (worshippers whose claims are suppressed), active enforcement (worship bans, security), and asymmetric benefit concentration (tourism revenue, secular ideology) prevent this misclassification. It is not a Piton because beneficiaries are concentrated and actively profit; it is not a Snare because the coordination function (preservation, scholarship) is structurally real and not merely cover. Tangled Rope captures the hybrid: genuine coordination fused with extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heritage_preservation_vs_worship_exclusion,
    'Is the suppression of worship an unavoidable cost of heritage preservation, or does the heritage frame legitimate the exclusion of specifically Islamic claims?',
    'Comparative analysis of heritage sites that permit worship alongside tourism; if preservation outcomes hold under shared-use regimes, the exclusion is not structurally necessary.',
    'If exclusion is unnecessary, the coordination story is separable from the suppression mechanism, clarifying the extraction component; if necessary, part of the measured extraction is the price of genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritage_preservation_vs_worship_exclusion, conceptual, 'Whether worship suppression is inherent to heritage preservation or instrumental to extraction.').

omega_variable(
    secular_ideology_naturalization,
    'Is the universal heritage frame a genuine transcendence of particularism, or does it naturalize Kemalist secular-modernist ideology as neutral?',
    'Historical genealogy of the museum decree and its ideological deployment in Turkish republican discourse; examination of whether ''neutrality'' consistently maps onto secularist policy preferences.',
    'If the frame naturalizes a specific ideology, the constraint''s accessibility_collapse is partly an artifact of ideological capture rather than objective heritage necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_ideology_naturalization, conceptual, 'Whether universal heritage neutrality masks a specific secularist ideology.').

omega_variable(
    cs_authority_grounding_ambiguity,
    'Does the technocratic administration''s authority derive from heritage expertise or from state extraction of tourism revenue and ideological legitimacy?',
    'Counterfactual test: whether the authority would persist if tourism revenue and secularist ideological benefits were removed, leaving only preservation costs.',
    'If extraction is primary, the CS authority_grounding is extraction and the constraint''s persistence is rent-dependent; if expertise, the grounding is expertise and the coordination is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_grounding_ambiguity, conceptual, 'Alternative framings of the commitment system''s authority grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 86).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hagi_tr_t14, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement(hagi_tr_t28, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 28, 0.35).
narrative_ontology:measurement(hagi_tr_t42, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 42, 0.4).
narrative_ontology:measurement(hagi_tr_t56, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 56, 0.42).
narrative_ontology:measurement(hagi_tr_t70, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 70, 0.44).
narrative_ontology:measurement(hagi_tr_t86, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 86, 0.45).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hagi_be_t14, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(hagi_be_t28, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement(hagi_be_t42, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement(hagi_be_t56, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 56, 0.72).
narrative_ontology:measurement(hagi_be_t70, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 70, 0.75).
narrative_ontology:measurement(hagi_be_t86, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 86, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hagi_su_t14, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(hagi_su_t28, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(hagi_su_t42, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(hagi_su_t56, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 56, 0.76).
narrative_ontology:measurement(hagi_su_t70, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 70, 0.8).
narrative_ontology:measurement(hagi_su_t86, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 86, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hagia_sophia_substrate kernel, decomposed per the epsilon-invariance principle because the colloquial label 'Hagia Sophia' conflates three structurally distinct legitimacy claims with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
