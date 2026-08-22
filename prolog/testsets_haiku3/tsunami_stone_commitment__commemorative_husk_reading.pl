% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Stone Inscriptions as Commemorative Husks (Non-Protective Reading)
 *   domain: disaster_anthropology/institutional_memory
 *
 * SUMMARY:
 *   This is the commemorative-husk reading of the tsunami stone commitment
 *   kernel. The reading asserts that tsunami stone inscriptions—markers
 *   placed in Japanese coastal villages after historic tsunamis to warn
 *   future generations—have decayed from operative behavioral guides into
 *   symbolic artifacts maintained for tourism and institutional heritage
 *   prestige. Under this reading, the original protective force of the stones
 *   (intergenerational transmission of 'do not build below this line') was
 *   abandoned or weakly enforced as coastal zones underwent economic
 *   development. Development actors benefit from this reframing because it
 *   allows building in original safe zones without confronting the stones'
 *   original message. Future residents pay the extraction cost by inheriting
 *   developed coastlines vulnerable to tsunami, having lost the protective
 *   knowledge the stones were designed to transmit. The 2011 Tōhoku tsunami
 *   empirically tested both readings: areas where the behavioral-competence
 *   reading held (villages that maintained active norm transmission) had
 *   better survival outcomes than areas where the commemorative reading held
 *   (zones that had reframed stones as museums rather than warnings).
 *
 * KEY AGENTS:
 *   - coastal_development_actors — institutional beneficiaries extracting economic value from coastal zone expansion; d ≈ 0.0–0.2
 *   - municipal_administrators — agenda-setters who control the stones' institutional framing; d ≈ 0.3–0.5 (constrained between heritage preservation duty and development pressure)
 *   - intergenerational_transmitters — excluded from the constraint's operation; would maintain the protective norm if seated; d ≈ 0.9 (forced targets of the extractive reading)
 *   - future_coastal_residents — victims bearing the non-protection cost; d ≈ 1.0 (trapped)
 *   - modern_tsunami_science — observers validating the stones' original risk identification but not steering their use; d ≈ 0.5 (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.82).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.71).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, resistance, 0.39).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Stone Inscriptions as Commemorative Husks (Non-Protective Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "disaster_anthropology/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, '4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1').
narrative_ontology:cs_kernel_codification('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', fixed_text).
narrative_ontology:cs_authority_grounding('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', extraction).
narrative_ontology:cs_reading_relation('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', foundational, stones_decayed_to_symbols).
narrative_ontology:cs_axiom_status(stones_decayed_to_symbols, holdable).
narrative_ontology:cs_axiom_grounding('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', stones_decayed_to_symbols, empirically_contingent).
narrative_ontology:cs_axiom('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', foundational, protective_transmission_abandoned).
narrative_ontology:cs_axiom_status(protective_transmission_abandoned, holdable).
narrative_ontology:cs_axiom_grounding('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', protective_transmission_abandoned, empirically_contingent).
narrative_ontology:cs_reference_frame('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', institutional_heritage_stewardship).
narrative_ontology:cs_drift_state('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', contemporary_post_2011, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4ff6fc12-5efb-4a6a-8d4c-2f9d712aeba1', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, municipal_administrators).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, heritage_preservation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Real-estate developers, municipal tax-revenue officers, and construction companies benefit from the constraint's operation: coastal zones can be developed and expanded without confronting or enforcing the original stone-based restrictions. They collect the economic value from zone expansion and higher land prices.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors, beneficiary,
    powerful, biographical, arbitrage, national).

% Inherit the stones and the mandate to preserve cultural heritage. They face conflicting pressures: conservation duty (keep the artifacts intact), development pressure (allow coastal expansion), and budget constraints (heritage preservation costs money). They resolve the conflict by reframing the mandate as physical preservation of the artifact rather than behavioral transmission of the protective message. They do not actively transmit the protective norm to new generations; they maintain the stones as tourist sites and historical documents.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, municipal_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Village elders, local historians, and community educators who would traditionally transmit the stones' protective meaning across generations are structurally excluded from the constraint's operation. Municipal heritage administrators do not consult them; they treat the stones as institutional artifacts rather than as living community practice. The transmitters are trapped in their communities (cannot exit the geographic/cultural context) and cannot change the frame from commemorative back to protective without institutional power.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, intergenerational_transmitters, excluded,
    moderate, generational, trapped, local).

% Inherit developed coastal zones and the loss of the protective knowledge the stones were designed to transmit. They face tsunami risk in populated areas that the original stones warned against. Their exit is structurally unavailable: they are born into a geographic and institutional arrangement shaped by the commemorative reframing and cannot choose to have been born into a differently developed coastline.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, civilizational, trapped, local).

% Benefit from the stones' reframing as cultural artifacts requiring preservation rather than as operative behavioral guides requiring transmission. This framing secures heritage funding and allows them to frame stone-preservation work as culturally important while avoiding the burden of enforcing the original protective mandate across generations.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, heritage_preservation_advocates, beneficiary,
    moderate, biographical, constrained, national).

% Produce independent evidence validating that the stones' placement correctly identified tsunami-vulnerable zones. Their analysis confirms the stones' original message was empirically sound. However, they sit outside the institutional mechanism that determines whether the stones' message is transmitted or buried. Their evidence does not feed into municipal planning or heritage administration decisions about the stones.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, modern_tsunami_scientists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a heritage preservation mechanism and historical record of past tsunamis; allows communities to acknowledge historical catastrophe risk through institutional commemoration.
% TRANSFER_FUNCTION: Transfers the protective knowledge embedded in stone placement from active intergenerational practice to institutional heritage stewardship; future generations inherit developed coastlines (economic gain to development actors) rather than inherited caution (the original protective message).
% ABSENT_VOICES: Village-based intergenerational transmitters and behavioral-competence advocates are excluded from municipal heritage administration and coastal planning processes. They would argue for treating the stones as operative behavioral guides requiring active transmission, but they are not seated in the decision-making structures.
% DISAPPEARANCE_RATIONALE: Under the commemorative reading, if the stones disappeared the world would be largely unchanged: coastal development would continue as it has, and only a tourist site and heritage monument would be lost. Under the behavioral-competence reading, if the stones disappeared it would represent a final severing of a living protective tradition, but the damage (loss of protective norm transmission) has already occurred through non-enforcement.
% FOUNDING_PROBLEM: Communities in tsunami-prone regions needed a durable, intergenerationally transmissible record of catastrophe risk and safe-zone boundaries so that future inhabitants would inherit the warning without requiring continuous institutional memory.
% FOUNDING_PROBLEM_CORROBORATION: Municipal administrators, heritage preservation agencies, and coastal development interests all attest the founding problem is substantially solved (we have the historical record, the stones are preserved, the knowledge is available). The 2011 Tōhoku tsunami and modern tsunami science attest the founding problem remains live (catastrophes recur, the risk is not gone)—but these attesting voices (disaster researchers, survivors, intergenerational transmitters) are excluded from the institutional reframing that declared the problem dead. The death of the problem is asserted by the benefiting parties; corroboration from outside the beneficiary set contradicts this verdict.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__commemorative_husk_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.82 by the interval end because the constraint now operates as a mechanism that transfers protective knowledge away from future residents and toward current development interests. Early in the interval (1885), the stones retained behavioral force and extraction was minimal (0.15); by 1970, municipal institutionalization had begun reframing them as heritage (0.38); by 2011 the reframing was near-complete and the 2011 tsunami revealed the extraction cost (0.75). The measurement series tracks this transition from operative norm to symbolic artifact. Theater ratio climbs from 0.08 to 0.68, indicating the growing share of enforcement activity devoted to physical preservation rather than behavioral transmission. Suppression is substantial (0.71) because maintaining the commemorative reframing requires active exclusion of intergenerational transmitters and behavioral-competence advocates from planning discourse. The constraint's persistence depends on suppressing the alternative reading, not on participant preference for heritage over protection.
 *
 * PERSPECTIVAL GAP:
 *   Development actors compute the constraint as a near-rope (coordination benefit for knowing historical risk; low cost of preserving artifacts). Municipal administrators compute it as theatrical maintenance of cultural assets (moderate coordination, manageable enforcement). Future residents would compute it as a snare (non-protection extraction, trapped exit). The engine, reading from the structural data (excluded intergenerational voices, non-enforcement of original message, beneficiary externalization to future generations), computes a piton where the original function (protective norm transmission) has atrophied and persists only through institutional performance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations plus exit options. Coastal development actors are beneficiaries with powerful institutional position and arbitrage options (they can invest elsewhere if coastal zones tighten; they are not trapped). Directionality for them approaches 0.0–0.2: the constraint subsidizes them by allowing development they would otherwise face resistance on. Future coastal residents are victims, powerless, with trapped exit (they are born into the geography shaped by this constraint's operation): directionality approaches 1.0 for them. Municipal administrators are constrained (heritage duty binds them) but moderate power (they can resist development pressure selectively): directionality around 0.3–0.5. Intergenerational transmitters are excluded rather than directly targeted: the constraint operates by keeping them out of the conversation, not by imposing costs on them directly (though their professional capacity to transmit knowledge is suppressed). The extraction mechanism is structural: the reframing from behavioral to commemorative silently shifts the burden of maintaining awareness from living practice to institutional performance, which eventually fails when enforcement attention erodes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy: the founding mandate (intergenerational transmission of tsunami-risk knowledge) has outlived its institutional support. Modern municipal administrators inherit the stones and the mandate, but face competing pressures (development, budgets, heritage vs. safety). Rather than admit the mandate conflict, they reframe the mandate as heritage preservation (keep the artifact intact) rather than protective transmission (keep the knowledge alive across generations). The reframing allows them to fulfill the ceremonial mandate (preserve the stone) while abandoning the functional mandate (enforce the protective message). Theater_ratio's rise from 0.08 to 0.68 tracks this mandatrophy: the majority of effort spent on the stones by 2024 is ceremonial (tourism, historical documentation, physical conservation) rather than functional (warning future residents). The 2011 tsunami revealed the mandatrophy acutely: the stones predicted the hazard correctly, but communities that had reframed them as heritage markers suffered higher casualty rates than communities that maintained them as active behavioral guides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Are the commemorative and behavioral-competence readings logically foreclosive (mutually exclusive within a single framework), or do they coexist as live positions held by different institutional actors and communities?',
    'Examine whether any single community, institution, or authority structure holds both readings simultaneously, or whether the readings are cleanly partitioned across actors. The 2011 tsunami data suggests villages held the behavioral reading and exhibited protective behavior; municipalities held the commemorative reading and exhibited heritage-preservation behavior—suggesting coexistence rather than foreclosure.',
    'If foreclosive, one reading''s dominance implies structural elimination of the other. If coexistent, the constraint''s classification depends on which reading''s institutional adoption is dominant (currently: municipal/governmental adoption of commemorative, community-level persistence of behavioral). The engine gates foreclosure on cs_axiom_contradiction; axioms must reflect which reading forecloses, if any.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between the two readings.').

omega_variable(
    institutional_mandate_reframing,
    'Did municipal administrators consciously reframe the stones'' mandate from protective to commemorative, or did the reframing occur as an emergent consequence of institutional resource scarcity and development pressure, with no explicit agency to change the mandate?',
    'Archival evidence of policy decisions, interviews with administrators spanning decades, examination of heritage-preservation legislation and its timing relative to coastal development expansion.',
    'If conscious reframing (agency), the piton classification stands and the extraction is intentional. If emergent (drift), the piton remains structurally true but the seat-level blame/intention analysis changes; future-generation extraction is an unintended consequence of institutional constraints rather than designed transfer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_mandate_reframing, empirical, 'Intentionality of the commemorative reframing.').

omega_variable(
    intergenerational_transmission_salvageability,
    'Can the behavioral-competence reading''s protective function be recovered at institutional scale, or has the interval of non-transmission eroded the cultural capacity to reactivate the norm across generations?',
    'Post-2011 case studies of communities attempting to reintegrate stone-based warnings into disaster education and coastal zoning; measurement of transmission success rates in regions with explicit re-activation programs vs. commemorative-only regions.',
    'If salvageable, the constraint could transition from piton to rope through institutional redesign (recover the protective mandate). If eroded, the non-transmission interval has locked in a new state: future residents inherit the cultural amnesia as well as the stones, and recovery requires external intervention (new inscriptions, legal mandates, scientific re-authorization). The timescale of recovery becomes civilizational rather than biographical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_transmission_salvageability, empirical, 'Whether protective intergenerational transmission can be recovered post-decay.').

omega_variable(
    extraction_mechanism_mechanism,
    'Is the extraction mechanism structural (the stones physically exist but institutional reframing strips them of behavioral force, such that even perfect knowledge of the stones would not alter future-resident behavior because the institutional context makes them inert) or epistemic (the protective knowledge exists in the stones but future residents simply never learn it, and transmission failure is contingent rather than structural)?',
    'Thought experiment: if a future resident learned the stone''s original message perfectly, would institutional and social context allow them to act on it (structural extraction), or would they have the option to act on it (epistemic failure)? Evidence from post-2011 communities: do residents who re-learned the stone messages subsequently change settlement and building behavior? If yes, extraction is epistemic and reversible. If no, extraction is structural and locked in by institutional design.',
    'If structural, the extraction persists regardless of information accessibility; the constraint''s persistence depends on institutional silence about the stones'' operative force. If epistemic, recovery requires information transmission plus institutional support for behavioral change. Classification implications: structural extraction is more snare-like (coercive system design); epistemic failure is more piton-like (inert institutional maintenance). The distinction affects repair strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_mechanism, conceptual, 'Whether extraction is structural (embedded in institutional design) or epistemic (contingent on information failure).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 1885, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t1885, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1885, 0.08).
narrative_ontology:measurement(tsun_tr_t1945, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(tsun_tr_t1970, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement(tsun_tr_t1995, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1995, 0.52).
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.64).
narrative_ontology:measurement(tsun_tr_t2024, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 2024, 0.68).

% Extraction over time
narrative_ontology:measurement(tsun_be_t1885, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1885, 0.15).
narrative_ontology:measurement(tsun_be_t1945, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1945, 0.22).
narrative_ontology:measurement(tsun_be_t1970, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(tsun_be_t1995, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.75).
narrative_ontology:measurement(tsun_be_t2024, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t1885, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1885, 0.35).
narrative_ontology:measurement(tsun_su_t1945, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1945, 0.41).
narrative_ontology:measurement(tsun_su_t1970, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(tsun_su_t1995, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1995, 0.64).
narrative_ontology:measurement(tsun_su_t2011, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(tsun_su_t2024, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__commemorative_husk_reading, 0.15).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel decomposes into two constraint stories with structurally distinct ε values and beneficiary/victim sets. The behavioral_competence_reading models the stones as retaining operative protective force (low ε, beneficiary = future residents). This commemorative_husk_reading models the stones as decayed to symbols (high ε, beneficiary = development actors, victim = future residents). The readings coexist: some communities instantiate behavioral competence, others instantiate commemorative husks. They are linked as sibling readings of a single contested kernel. The 2011 Tōhoku tsunami provided empirical differentiation: casualty and survival patterns correlated with which reading a community instantiated. These are not observer-dependent measurements of a single constraint; they are two distinct constraints with different structural functions operating in overlapping geographic/social space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__commemorative_husk_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
