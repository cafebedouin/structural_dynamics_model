% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Tsunami Warning Stone as Commemorative Husk (Land-Use Prohibition Decayed to Symbolic Marker)
 *   domain: disaster anthropology / institutional memory / land-use governance
 *
 * SUMMARY:
 *   This story instantiates the commemorative_husk reading of the
 *   stone_land_use_rule kernel: a physical tsunami-warning marker whose
 *   original inscribed instruction ('do not build below this line') has,
 *   within a few generations, lost all operative connection to actual
 *   building decisions. The stone still stands, is still maintained, is still
 *   the object of an annual remembrance ceremony — but it now functions
 *   purely as a heritage artifact. Waterfront construction proceeds seaward
 *   of the marker as a matter of routine, uncoupled from the stone's
 *   geographic warning. This is the high-epsilon, zero-land-use-constraint
 *   reading: the sibling behavioral_competence reading, where the stone still
 *   structures daily spatial practice and functions as a lived,
 *   low-extraction Rope, is a different constraint entirely, authored
 *   separately and linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - coastal_tourism_developers: primary beneficiary (powerful/arbitrage) — profits from unrestricted waterfront land
 *   - waterfront_property_sellers: beneficiary (moderate/mobile) — sells premium land the stone once would have restricted
 *   - municipal_tax_authorities: beneficiary and agenda_setter (institutional/constrained) — collects tax revenue from development, funds only the ceremonial upkeep of the stone
 *   - future_waterfront_residents: primary victim (powerless/trapped) — inherits undisclosed hazard risk
 *   - generational_memory_holders: victim, excluded (powerless/trapped, civilizational horizon) — holds the knowledge the stone encoded but has no regulatory voice
 *   - disaster_historians: analytical observer — documents the cross-site pattern of warning-to-heritage decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.71).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.12).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.86).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.71).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.86).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Tsunami Warning Stone as Commemorative Husk (Land-Use Prohibition Decayed to Symbolic Marker)").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster anthropology / institutional memory / land-use governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '6f515af6-abf9-4443-9e3b-4c4b3a6b1186').
narrative_ontology:cs_kernel_codification('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', fixed_text).
narrative_ontology:cs_authority_grounding('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', practice).
narrative_ontology:cs_reading_relation('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', foundational, commemorative_function_supersedes_regulatory_function).
narrative_ontology:cs_axiom_status(commemorative_function_supersedes_regulatory_function, holdable).
narrative_ontology:cs_axiom_grounding('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', commemorative_function_supersedes_regulatory_function, conventional).
narrative_ontology:cs_axiom('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', secondary, memory_preservation_satisfies_founding_obligation).
narrative_ontology:cs_axiom_status(memory_preservation_satisfies_founding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', memory_preservation_satisfies_founding_obligation, conventional).
narrative_ontology:cs_reference_frame('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', post_disaster_inscribed_prohibition).
narrative_ontology:cs_drift_state('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', contemporary_waterfront_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6f515af6-abf9-4443-9e3b-4c4b3a6b1186', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, coastal_tourism_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_property_sellers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_tax_authorities).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_waterfront_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, generational_memory_holders).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, ancestral_disaster_warnings_deserve_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build hotels, vacation rentals, and waterfront amenities below the historical high-water marker lines because the stones carry no zoning force. The stone's continued presence as a photographed heritage object is actively useful to them as a marketing asset for the very land it once warned against building on.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_tourism_developers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Sell and lease lots seaward of the stones at premium prices precisely because the land is scenic and now unencumbered by any enforceable restriction. They benefit from the stone's symbolic status without bearing any of the restriction it once implied.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_property_sellers, beneficiary,
    moderate, biographical, mobile, local).

% Collect higher property and lodging tax revenue from waterfront development that the stone's original prohibition would have prevented. They maintain the stone as a heritage site (funding a small plaque, an annual remembrance ceremony) while declining to codify its warning into enforceable setback law, because doing so would shrink the taxable waterfront footprint.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_tax_authorities, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, municipal_tax_authorities, agenda_setter).

% Purchase or rent homes and businesses on land the stone was erected to keep clear, without knowing — or without being able to act on — the disaster-risk information the marker encodes. They bear the eventual physical risk with none of the decision-making power over how the stone's warning was allowed to lapse into decoration.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_waterfront_residents, payer,
    powerless, generational, trapped, local).

% Elders and descendants of prior disaster survivors who understand the stone's original behavioral meaning watch the warning become an object of tourist photography and civic ceremony rather than a lived spatial rule. Their testimony about what the stone meant is solicited for heritage documentation but not translated into planning authority.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, generational_memory_holders, payer,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, generational_memory_holders, excluded).

% Could, in principle, convert the stone's marked elevation into a legal building setback line, but land-use authority sits with a different office than the heritage-preservation office that maintains the stone, and no mechanism currently routes the stone's geographic information into zoning maps.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_planning_department, excluded,
    moderate, biographical, constrained, local).

% Document the pattern across multiple tsunami-warning-stone sites: markers erected after a disaster with explicit behavioral instructions ('do not build below this point') that, within two to four generations, are reclassified by local institutions as historical/commemorative rather than regulatory, coincident with rising waterfront land values.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, municipal_tax_authorities).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its original form, the stone coordinated multi-generational settlement decisions around a known hazard line without requiring ongoing technical monitoring — a single physical marker let every future resident read the safe boundary without needing tsunami science or bureaucratic zoning apparatus. In its current form, no coordination function operates on land use; the residual coordination is purely commemorative (coordinating a shared ceremony date and a shared object of remembrance).
% TRANSFER_FUNCTION: Moves disaster risk from the present (where it could be priced into land-use decisions and reduced by refusing to build) onto future residents and their descendants, while moving development profit from foregone (in the enforced reading) to realized (in this reading) for developers, sellers, and tax authorities in the present.
% ABSENT_VOICES: Future waterfront residents are not yet resident and cannot object to a risk they don't know they are inheriting. Generational memory holders who know what the stone meant are consulted for heritage narrative but excluded from the zoning conversation where their knowledge would have regulatory teeth.
% DISAPPEARANCE_RATIONALE: If the stone itself vanished, land-use decisions would not change at all under this reading — building already proceeds independent of the stone's location, its warning having no operative force. Only the ceremony and the heritage-tourism draw would be lost; the actual arrangement of coastal development is already indifferent to the marker's presence.
% FOUNDING_PROBLEM: Coastal communities historically lacked any durable, illiterate-accessible, multi-generational mechanism for transmitting empirical disaster-risk knowledge (specifically, observed tsunami run-up elevation) into binding settlement behavior, so survivors erected physical markers with inscribed instructions not to build below them.
% FOUNDING_PROBLEM_CORROBORATION: Seismologists and coastal-hazard researchers (outside the tax authority and developer beneficiary set) attest the physical hazard the stone documents remains live and unchanged — the tsunami run-up elevation the stone marks is still the empirically relevant risk line. Disaster historians studying comparable marker sites corroborate that the founding problem was never resolved by any substitute mechanism; the stone's regulatory function lapsed while the hazard it recorded did not.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_unchanged).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high and rising (0.10 to 0.71 across the interval) because the gap between the stone's original behavioral force and current land use widens steadily as memory of the founding disaster fades across generations and waterfront land value climbs. Theater ratio is authored even higher and rising faster (0.05 to 0.86) because the proportion of institutional attention devoted to the stone that is purely commemorative — plaques, ceremonies, heritage grants — grows exactly as the proportion devoted to actual land-use enforcement (already at zero in this reading) stays flat at nothing. Suppression is authored low (0.12) because this reading does not depend on coercion: no one is forced to build near the water, and no one is forced to ignore the stone. The extraction here is achieved through drift and omission, not enforcement — the mechanism is a permissive silence in the zoning apparatus, not an active suppressive structure. Accessibility collapse is low (0.20): alternative building sites and enforceable-setback ordinances remain fully available and were never foreclosed by any coercive mechanism, only by institutional inattention. Resistance is low (0.15): because the extraction is invisible (framed as heritage preservation, not land-use failure), it draws little organized pushback until after a disaster event recurs.
 *
 * PERSPECTIVAL GAP:
 *   From the municipal tax authority's seat, the stone is a heritage success story — preserved, ceremonially honored, drawing modest cultural tourism. From the future waterfront resident's seat (who does not yet occupy that seat and cannot yet object), the same arrangement is an undisclosed transfer of catastrophic risk. The engine should compute these as structurally different experiences of the identical artifact: the agenda_setter/beneficiary seat sees successful commemoration; the payer seat, once populated, would see an inherited hazard that institutional silence normalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal tourism developers and property sellers are declared beneficiaries with arbitrage/mobile exit — they can develop, sell, and leave before any recurrence, giving them low derived directionality (near full-beneficiary). Municipal tax authorities are beneficiaries with constrained exit (an institution cannot relocate) but institutional power lets them shape which risks get zoned and which get merely commemorated — d sits low-moderate. Future waterfront residents and generational memory holders are declared victims with trapped exit and powerless standing — the derivation should push their directionality toward the full-target end, since they bear costs they did not choose and cannot leave once the hazard materializes (residents) or cannot convert their knowledge into protective force (memory holders).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting empirical hazard knowledge into binding settlement behavior — is authored as still live (seismic risk is unchanged), yet the stone's original mechanism for solving it has been superseded by an ornamental one. This is a textbook Piton signature: the mandate (behavioral prohibition) has been replaced by a mandate-shaped performance (commemoration) without anyone declaring the original mandate abandoned. Classifying this as Piton rather than Mountain prevents mislabeling the decayed prohibition as a settled natural fact of coastal life, and classifying it as Piton rather than Snare correctly reflects that no single concentrated beneficiary actively enforces the silence — the extraction is diffuse across developers, sellers, and a passive tax authority, sustained by institutional inertia (heritage office vs. planning office silo) rather than deliberate coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_is_locally_true,
    'For any given real stone-marker site, is the community''s actual current relationship to the marker closer to commemorative_husk (this reading) or behavioral_competence (the sibling reading) — and can this be determined empirically rather than asserted by whichever party benefits from the answer?',
    'Site-specific survey of actual building permits and construction issued seaward of the stone''s marked elevation over the past two generations, cross-referenced against whether local zoning code cites the stone as a legal reference line. A site with zero permits issued below the line and zoning code citation is behavioral_competence; a site with routine permits and no zoning citation is commemorative_husk.',
    'If empirical survey shows a given site has, in fact, maintained behavioral compliance, this story''s high-epsilon commemorative_husk classification does not apply to that site and the sibling reading''s Rope classification is the structurally correct one there instead — the kernel resolves differently per site, not universally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_locally_true, empirical, 'Whether any given stone site is empirically in the commemorative_husk or behavioral_competence state.').

omega_variable(
    decay_mechanism_natural_or_engineered,
    'Did the warning''s behavioral force decay through ordinary intergenerational memory loss (a natural epistemic process), or was the decay actively facilitated by institutional actors who benefited from the land becoming buildable (e.g., heritage-office reclassification timed to coincide with rezoning pressure)?',
    'Archival review of municipal planning-department and heritage-office records to see whether the formal reclassification of the stone from ''boundary marker'' to ''historic monument'' preceded, followed, or coincided with waterfront rezoning applications or land-value spikes.',
    'If reclassification timing tracks rezoning pressure, this is evidence the husk state is a constructed extraction dressed as natural forgetting, strengthening the case for Piton (or even Snare, if a specific concentrated actor is shown to have engineered the decay) over an innocent-drift account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decay_mechanism_natural_or_engineered, empirical, 'Whether the warning''s decay was organic memory loss or institutionally facilitated.').

omega_variable(
    stakeholder_framing_alternative,
    'Should municipal_tax_authorities be read as the primary gain-recipient (as authored here) or should the analysis instead center coastal_tourism_developers as the true capturer, with the tax authority merely a downstream secondary beneficiary passively collecting incidental revenue?',
    'Compare relative revenue magnitudes and political influence: track whether tax-authority zoning inaction was independently motivated (revenue-seeking) or was itself the product of developer lobbying, which would make developers the primary capturer and the tax authority a captured intermediary.',
    'If developers are shown to be the true capturer via lobbying capture of the tax authority, gain_flow should be re-authored to name coastal_tourism_developers rather than municipal_tax_authorities, and the classification would shift toward regulatory-capture Tangled Rope rather than diffuse-inertia Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_framing_alternative, conceptual, 'Alternative framing of which stakeholder is the true gain-recipient behind the zoning silence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.32).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.5).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.66).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.78).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.86).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__commemorative_husk, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, information_standard).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.03).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% This story (commemorative_husk) and stone_land_use_rule__behavioral_competence are the two readings of the single stone_land_use_rule kernel. They share the identical physical artifact and inscribed text but diverge completely on epsilon: behavioral_competence authors near-zero extraction (the prohibition is live and observed, functioning as a genuine Rope), while this story authors substantial and rising extraction (the prohibition has decayed to ceremony while development proceeds unconstrained, functioning as a Piton with extractive drift). Per the ε-invariance principle, these are not two measurements of one constraint but two structurally distinct constraints linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
