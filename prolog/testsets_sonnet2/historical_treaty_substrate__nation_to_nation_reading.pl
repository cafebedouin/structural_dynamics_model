% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historic Treaties Read as Nation-to-Nation International Agreements
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the nation-to-nation reading of the historical
 *   treaty substrate: historic treaties between settler states and Indigenous
 *   nations are read as international agreements between sovereign equals,
 *   requiring ongoing consent to territorial and jurisdictional changes and
 *   governed by treaty-law principles (good faith, no unilateral
 *   modification, continuing consent) rather than as a single completed
 *   transaction. Under this reading, Indigenous nations sit as co-equal
 *   sovereigns with consent rights, and unilateral resource extraction or
 *   jurisdictional assertion by the settler state constitutes a treaty
 *   violation. The ε here measures the standing arrangement AS PRACTICED —
 *   the settler state's domestic incorporation of treaty relations that falls
 *   well short of full nation-to-nation consent enforcement — not the fully
 *   consent-respecting arrangement this reading would endorse. Sibling
 *   readings (extinguishment_reading: treaties as completed cession
 *   transactions; stewardship_reading: relational pacts with no sovereignty
 *   cession) are separate constraint stories with their own ε and stakeholder
 *   structures, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - settler_state_apparatus: agenda-setter/beneficiary, administers treaty interpretation and resource permitting through domestic law while claiming international-law legitimacy
 *   - resource_extraction_industry: beneficiary, obtains extraction rights mediated by domestic rather than genuinely bilateral consent
 *   - indigenous_treaty_nations: payer, holds the consent right this reading recognizes but cannot enforce it outside the settler state's own courts
 *   - domestic_judiciary: observer/agenda-setter, determines how much of the international-law framing actually operates
 *   - international_treaty_bodies: excluded, would be the natural adjudicator under this reading but lacks binding jurisdiction
 *   - settler_descendant_landholders: beneficiary, holds title whose provenance this reading would subject to renewed scrutiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.68).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.71).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historic Treaties Read as Nation-to-Nation International Agreements").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, 'e556aca8-ce46-4246-8ea6-7770f7bb3223').
narrative_ontology:cs_kernel_codification('e556aca8-ce46-4246-8ea6-7770f7bb3223', fixed_text).
narrative_ontology:cs_authority_grounding('e556aca8-ce46-4246-8ea6-7770f7bb3223', lineage).
narrative_ontology:cs_interpretation_layer_present('e556aca8-ce46-4246-8ea6-7770f7bb3223').
narrative_ontology:cs_reading_relation('e556aca8-ce46-4246-8ea6-7770f7bb3223', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('e556aca8-ce46-4246-8ea6-7770f7bb3223', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('e556aca8-ce46-4246-8ea6-7770f7bb3223', foundational, treaty_parties_are_coequal_sovereigns).
narrative_ontology:cs_axiom_status(treaty_parties_are_coequal_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('e556aca8-ce46-4246-8ea6-7770f7bb3223', treaty_parties_are_coequal_sovereigns, conventional).
narrative_ontology:cs_axiom('e556aca8-ce46-4246-8ea6-7770f7bb3223', foundational, territorial_change_requires_ongoing_consent).
narrative_ontology:cs_axiom_status(territorial_change_requires_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('e556aca8-ce46-4246-8ea6-7770f7bb3223', territorial_change_requires_ongoing_consent, deontological).
narrative_ontology:cs_reference_frame('e556aca8-ce46-4246-8ea6-7770f7bb3223', bilateral_sovereign_treaty_relations).
narrative_ontology:cs_drift_state('e556aca8-ce46-4246-8ea6-7770f7bb3223', contemporary_domestic_incorporation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e556aca8-ce46-4246-8ea6-7770f7bb3223', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_apparatus).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industry).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_descendant_landholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers land title, resource permitting, and treaty interpretation through domestic courts and legislatures. Under this reading it is bound by international treaty-law principles — ongoing consent, good faith, no unilateral abrogation — but in practice continues to authorize resource extraction and land use changes on treaty territory through domestic statute, treating the treaty as internally incorporated rather than internationally binding. It can rewrite the domestic legal test for consultation but bears none of the immediate cost of getting it wrong.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_apparatus, beneficiary).

% Obtains permits and licenses from the settler state for activity on treaty territory, structured as though the state alone has authority to grant them. Under a genuine nation-to-nation reading these permits would require the treaty nation's consent as a co-equal sovereign; industry's exposure to that requirement is currently mediated entirely through domestic consultation processes it can lobby to weaken.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industry, beneficiary,
    organized, biographical, mobile, national).

% Signed treaties understood, on this reading, as international agreements between sovereigns — obligating ongoing consent to territorial changes, not a one-time cession. In practice they must litigate or negotiate within the settler state's own courts to have that consent right recognized at all, and unilateral resource authorizations proceed on their territory without their agreement being treated as legally necessary. Exit from the settler state's domestic legal system is not realistically available; their leverage is litigation, direct action, and international forums (UN mechanisms, ILO conventions) that lack binding enforcement power over the settler state.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, payer,
    organized, civilizational, constrained, regional).

% Adjudicates treaty disputes using domestic constitutional and common-law frameworks, occasionally importing international treaty-law language (honor of the crown, fiduciary duty, good faith) without fully applying international law's consent and modification rules. Its rulings determine how much of the nation-to-nation reading gets operationalized versus how much remains rhetorical.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, domestic_judiciary, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, domestic_judiciary, agenda_setter).

% UN human rights mechanisms, ILO Convention 169 committees, and comparable international bodies would be the natural adjudicators if these instruments were treated as genuine international treaties, but the settler state does not recognize their jurisdiction as binding and excludes them from the domestic enforcement chain — their findings carry moral but not legal force.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_treaty_bodies, excluded,
    institutional, generational, analytical, global).

% Hold private title and resource rights on land within treaty territory, obtained through the domestic land system built on the extinguishment-adjacent administrative practice. Under the nation-to-nation reading, some of this title's provenance would be subject to renewed consent requirements they had no part in creating and would resist reopening.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_descendant_landholders, beneficiary,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formalizes ongoing government-to-government relations between the settler state and Indigenous nations, providing a durable framework for resolving disputes over land use, resource access, and jurisdiction without recourse to open conflict — the coordination value of a standing bilateral relationship.
% TRANSFER_FUNCTION: Under this reading, resource access and territorial authority should flow only with the treaty nation's ongoing consent; in the settler state's actual administrative practice, resource rents, land-use authority, and permitting revenue flow from treaty territory to the settler state and its licensees without that consent being treated as legally required.
% ABSENT_VOICES: International treaty-law bodies and adjudicators are structurally excluded from the enforcement chain the settler state actually uses; Indigenous treaty nations are formally party but their consent function is adjudicated by the very domestic courts of the state whose authority is in question.
% DISAPPEARANCE_RATIONALE: The settler state and industry would say little changes domestically if the nation-to-nation reading were abandoned, since it is not the operative reading in administrative practice today. Indigenous treaty nations would say its abandonment forecloses the strongest available legal basis for consent-based veto over resource extraction and confirms the extinguishment reading's practical dominance — a live and consequential difference, not a null one.
% FOUNDING_PROBLEM: The treaties were negotiated, on this reading, to establish lasting nation-to-nation relations between sovereign polities — securing peace, mutual recognition, and ongoing terms of coexistence and resource sharing rather than a single transfer of title.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous treaty nations and a body of international and comparative-law scholarship (citing the historical negotiating record, oral history, and analogues in international treaty practice) attest the nation-to-nation founding problem remains live and unresolved. The settler state's own administrative and judicial apparatus, whose position is contested here, treats the problem as substantially settled by domestic incorporation — that attestation comes from inside the benefiting party and is treated with corresponding weight.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, contested).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that resource rents and land-use authority continue to flow from treaty territory without the ongoing consent this reading holds to be legally required — a substantial gap between the reading's normative content and administrative practice. Suppression (0.71) is high because enforcing the consent right requires overcoming the settler state's monopoly on domestic adjudication and the absence of binding international enforcement; Indigenous nations face real structural barriers, not mere inconvenience. Theater ratio (0.40) captures that some genuine nation-to-nation language and consultation processes exist and are not purely cosmetic, but a rising share of institutional activity performs recognition without operationalizing consent as a veto. accessibility_collapse (0.5) and resistance (0.72) reflect that alternative readings and enforcement paths (litigation, international forums, direct action) remain genuinely available and actively pursued — this is not a fully closed system, which is why the type computes as tangled_rope rather than snare: there is a real coordination function (a durable bilateral framework preventing renewed conflict) alongside asymmetric extraction that requires active domestic enforcement to sustain.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state and resource industry are structural beneficiaries: they receive extraction authority and administrative discretion while incurring minimal enforceable cost from the consent requirement this reading asserts. Indigenous treaty nations are the target: they hold the normative right this reading names but face high suppression enforcing it, with exit constrained by lack of any parallel jurisdiction to appeal to. International treaty bodies are excluded rather than positioned as either beneficiary or victim — they are the absent adjudicator whose absence is structurally load-bearing for the settler state's practical dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing durable, consensual nation-to-nation relations — is genuinely contested rather than dead: Indigenous nations and comparative/international legal scholarship hold it live and unresolved, while the settler state's own apparatus treats it as substantially settled by domestic incorporation. This divergence is exactly the kind of mismatch (status=contested, corroboration split along the beneficiary line) the R5 genealogy interview is designed to surface: a founding-problem narrative that is affirmed by the reading's beneficiaries and denied by everyone else is a signal, not a resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    international_law_applicability,
    'Do historic treaties between Indigenous nations and settler states qualify as treaties under modern international law (Vienna Convention principles), or are they sui generis instruments whose classification is itself unsettled?',
    'Adjudication by an international tribunal with accepted jurisdiction, or a doctrinal consensus among international law scholars specifically addressing pre-Westphalian and non-state-to-state historical agreements.',
    'If genuinely international treaties, the settler state''s unilateral domestic incorporation is itself a breach of treaty-law principles, sharply raising measured extraction; if sui generis, the domestic incorporation approach may be defensible on different grounds, lowering it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_law_applicability, conceptual, 'Whether the nation-to-nation reading''s core legal premise is itself internationally recognized.').

omega_variable(
    reading_selection_evidence,
    'What in the historical negotiating record, oral history, and treaty text most supports choosing the nation-to-nation reading over the extinguishment or stewardship readings?',
    'Comparative analysis of treaty negotiation minutes, oral history from descendant communities, contemporaneous colonial administrative correspondence, and analogous international treaty practice from the same era.',
    'Strong evidence for nation-to-nation framing over extinguishment framing would support this reading''s consent-based veto claim; strong evidence for stewardship framing would suggest even the sovereignty-cession language in this reading overstates what was agreed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidence, conceptual, 'Documents the framing choice and evidentiary basis distinguishing this reading from its siblings, per the CS-framing under-determination guidance.').

omega_variable(
    settler_state_beneficiary_naturalization,
    'Does the settler state''s continued exercise of resource-permitting authority over treaty territory reflect settled, legitimate domestic sovereignty, or a naturalized continuation of the extraction this reading identifies as breach?',
    'Track whether new consultation and consent frameworks introduced over the measurement interval produce actual veto-capable outcomes for treaty nations, or remain advisory.',
    'If frameworks remain advisory, the settler state''s authority is better read as naturalized extraction inconsistent with this reading''s own premises; if veto-capable outcomes emerge, the tangled_rope classification would move toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_state_beneficiary_naturalization, empirical, 'Whether administrative practice is converging toward or diverging from the reading''s normative consent requirement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hist_tr_t8, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(hist_tr_t16, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(hist_tr_t24, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(hist_tr_t32, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(hist_be_t8, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(hist_be_t16, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(hist_be_t24, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(hist_be_t32, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hist_su_t8, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(hist_su_t16, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(hist_su_t24, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(hist_su_t32, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__nation_to_nation_reading, 0.1).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% One of three sibling readings of the historical_treaty_substrate kernel. extinguishment_reading treats the same treaty texts as completed cession transactions (lower measured Indigenous consent rights, higher settler-state discretion). stewardship_reading treats them as non-transactional relational pacts with no sovereignty cession at all (higher continuing Indigenous authority than even this reading). Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
