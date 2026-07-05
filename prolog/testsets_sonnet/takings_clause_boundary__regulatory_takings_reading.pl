% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Reading — 'Too Far' Diminution Doctrine (Penn Central / Mahon lineage)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint isolates the regulatory-takings reading of the Fifth
 *   Amendment's Takings Clause: the doctrinal line, originating in
 *   Pennsylvania Coal v. Mahon ('while property may be regulated to a certain
 *   extent, if regulation goes too far it will be recognized as a taking')
 *   and elaborated through Penn Central Transportation Co. v. New York City's
 *   multi-factor balancing test, holding that severe diminution in economic
 *   value — without any physical occupation or seizure — can itself
 *   constitute a compensable taking. This is deliberately NOT the same
 *   constraint as the physical-appropriation reading (which holds only direct
 *   seizure or permanent physical occupation triggers compensation) or the
 *   categorical-takings reading (which carves out per se rules for total
 *   wipeouts and physical occupations while relegating everything else to
 *   Penn Central). Those are separate constraints with separate ε values,
 *   linked here via network.affects_constraints. The regulatory-takings
 *   reading is structurally the most expansive and the most indeterminate of
 *   the three: it enlarges the victim class to include regulators and third
 *   parties bearing chilled-regulation externalities, and it substitutes an
 *   ad hoc, unpredictable balancing test for a bright line.
 *
 * KEY AGENTS:
 *   - property_owners_facing_severe_diminution: primary beneficiary of the doctrine's existence — gains a compensation remedy for non-physical takings
 *   - takings_litigation_bar: organized beneficiary of doctrinal indeterminacy itself
 *   - environmental_and_land_use_regulators: primary institutional payer — bears liability exposure and chilling effect
 *   - municipalities_with_constrained_budgets: fiscal payer, especially acute for small jurisdictions
 *   - neighboring_communities_bearing_externalities: powerless, excluded third-party payer of chilled-regulation costs
 *   - reviewing_courts: agenda-setter administering the ad hoc balancing test across decades
 *   - constitutional_scholars: analytical observer of the doctrine's costs and coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.52).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Reading — 'Too Far' Diminution Doctrine (Penn Central / Mahon lineage)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '8f122969-f509-46fe-9e33-da61fa6c358e').
narrative_ontology:cs_kernel_codification('8f122969-f509-46fe-9e33-da61fa6c358e', distributed).
narrative_ontology:cs_authority_grounding('8f122969-f509-46fe-9e33-da61fa6c358e', practice).
narrative_ontology:cs_interpretation_layer_present('8f122969-f509-46fe-9e33-da61fa6c358e').
narrative_ontology:cs_reading_relation('8f122969-f509-46fe-9e33-da61fa6c358e', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('8f122969-f509-46fe-9e33-da61fa6c358e', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('8f122969-f509-46fe-9e33-da61fa6c358e', foundational, severe_value_diminution_without_possession_is_compensable).
narrative_ontology:cs_axiom_status(severe_value_diminution_without_possession_is_compensable, holdable).
narrative_ontology:cs_axiom_grounding('8f122969-f509-46fe-9e33-da61fa6c358e', severe_value_diminution_without_possession_is_compensable, deontological).
narrative_ontology:cs_axiom('8f122969-f509-46fe-9e33-da61fa6c358e', secondary, ad_hoc_multifactor_balancing_is_the_appropriate_adjudicative_form).
narrative_ontology:cs_axiom_status(ad_hoc_multifactor_balancing_is_the_appropriate_adjudicative_form, holdable).
narrative_ontology:cs_axiom_grounding('8f122969-f509-46fe-9e33-da61fa6c358e', ad_hoc_multifactor_balancing_is_the_appropriate_adjudicative_form, instrumental).
narrative_ontology:cs_reference_frame('8f122969-f509-46fe-9e33-da61fa6c358e', mahon_confiscation_prevention_principle).
narrative_ontology:cs_drift_state('8f122969-f509-46fe-9e33-da61fa6c358e', post_penn_central_balancing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f122969-f509-46fe-9e33-da61fa6c358e', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners_facing_severe_diminution).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_rights_advocacy_organizations).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_and_land_use_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, municipalities_with_constrained_budgets).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, neighboring_communities_bearing_externalities).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, future_regulatory_beneficiaries).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, property_protection_extends_beyond_physical_possession).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, government_must_internalize_the_cost_of_regulatory_burden_concentration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own land or economic interests subjected to a regulation (wetlands designation, historic preservation overlay, zoning downgrade) that eliminates most or all economic use without any physical entry by the government. Under this reading they can sue for compensation by showing the regulation went 'too far,' converting what would otherwise be an uncompensated police-power burden into a compensable taking. Their exit from the regulatory burden runs through litigation, not through market alternatives.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners_facing_severe_diminution, beneficiary,
    moderate, biographical, constrained, national).

% Specialize in regulatory takings claims under the ad hoc balancing framework (economic impact, investment-backed expectations, character of governmental action). The doctrinal indeterminacy that resulted from rejecting a bright-line physical-only rule is their primary source of billable litigation; clearer rules in either direction would shrink the practice area.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar, beneficiary,
    organized, generational, arbitrage, national).

% Fund and coordinate strategic litigation to expand the regulatory-takings doctrine's reach, treating each favorable ruling as precedent that narrows the police power going forward. They are not directly harmed by any single regulation but benefit institutionally from the doctrine's existence and expansion.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Must now draft, defend, and sometimes abandon or water down regulations (wetlands protection, coastal setbacks, historic districts) because a sufficiently severe diminution-of-value claim can force compensation the regulating body cannot afford. The ad hoc balancing test gives them no ex ante certainty about which regulations will trigger liability, producing a documented chilling effect on regulatory ambition.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_and_land_use_regulators, payer,
    institutional, generational, constrained, national).

% Bear the direct fiscal exposure when a regulation is found to go 'too far' — compensation judgments come out of general funds or force settlement/repeal of the regulation. Smaller municipalities without deep legal budgets are effectively deterred from enacting land-use protections they cannot afford to defend or pay out on.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, municipalities_with_constrained_budgets, payer,
    institutional, generational, trapped, regional).

% Live adjacent to land that a regulation intended to protect (wetlands buffering flood risk, viewsheds, historic character) but where the regulation was withdrawn, weakened, or never enacted because of takings liability exposure. They absorb the externality — flooding, pollution, lost amenity value — with no seat at the takings litigation table and no compensation of their own.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, neighboring_communities_bearing_externalities, payer,
    powerless, biographical, trapped, local).

% The public that would benefit from regulations never enacted because of anticipated takings exposure. Diffuse, temporally displaced, and structurally absent from any individual takings case — no one represents 'the regulation that was never passed' in court.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, future_regulatory_beneficiaries, excluded,
    powerless, generational, trapped, national).

% Apply the ad hoc, multi-factor balancing test (Penn Central: economic impact, interference with investment-backed expectations, character of the governmental action) case by case. They administer and could in principle abandon or tighten this framework, but stare decisis and the absence of a workable bright-line alternative keep the indeterminate test in place across decades.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, reviewing_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Study the doctrine's coherence and consequences without a direct stake in any case outcome; document the chilling effect on regulation and the unpredictability costs the balancing test imposes on all sides.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, property_owners_facing_severe_diminution).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides property owners a mechanism to seek compensation when regulation, rather than physical seizure, destroys the practical value of their holding — preventing government from achieving de facto confiscation through regulatory means while avoiding the compensation obligation that would attach to outright seizure.
% TRANSFER_FUNCTION: When triggered, moves compensation from public treasuries (state/municipal) to individual property owners whose land was severely devalued by regulation; more diffusely, moves regulatory ambition and capacity away from land-use, environmental, and preservation goals because the doctrine's uncertainty raises the expected cost of regulating.
% ABSENT_VOICES: Neighboring communities who benefit from wetlands, flood buffers, and historic character have no standing in a takings case brought by the regulated landowner; future beneficiaries of regulations chilled by anticipated liability are, by definition, not represented anywhere in the record — the doctrine's costs are borne by parties who never appear in the litigation that produces them.
% DISAPPEARANCE_RATIONALE: If the regulatory-takings reading vanished and only the physical-appropriation reading governed, land-use, environmental, and preservation regulation could proceed with categorical certainty that non-physical value diminution never triggers compensation — regulators would recover discretion, litigation bar work in this area would collapse, and severely diminished property owners would lose their primary compensation remedy short of a categorical (Lucas-type) total wipeout.
% FOUNDING_PROBLEM: Pennsylvania Coal v. Mahon (1922) confronted the fear that government could accomplish through regulation what the Constitution forbids it from accomplishing through outright seizure — destroying nearly all economic value in land while paying nothing, simply by calling the destruction 'regulation' rather than 'taking.'
% FOUNDING_PROBLEM_CORROBORATION: Property rights scholars and the litigation bar attest the founding concern remains fully live — citing modern land-use regulations (wetlands designations, historic overlays, downzonings) that can eliminate nearly all value. Land-use and environmental law scholars, plus multiple state and municipal regulatory bodies testifying in amicus briefs, attest the doctrine as applied has drifted from preventing confiscatory abuse into a general-purpose tool for resisting ordinary land-use and environmental regulation, and that the indeterminate balancing test itself — not the underlying confiscation concern — is now the primary driver of litigation volume.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the doctrine transfers real fiscal exposure from regulatory bodies to property owners who successfully litigate severe diminution, while chilling regulation that would have protected diffuse third parties — a genuine transfer function, not merely rhetorical. Suppression (0.52) is moderate: the doctrine does not suppress regulators' formal authority to regulate, but it suppresses their practical willingness to regulate aggressively by attaching unpredictable compensation liability, and it offers property owners no self-help alternative outside protracted litigation. Theater ratio (0.28) is moderate-low — the doctrine performs genuine adjudicative work (courts do apply the Penn Central factors substantively) but a growing share of litigation activity is doctrinal maneuvering around an admittedly indeterminate standard rather than resolution of the underlying confiscation concern. Accessibility collapse (0.42) is middling: property owners retain other remedies (variance requests, as-applied challenges, political process) even where the takings claim fails, so alternatives have not fully collapsed. Resistance (0.68) is high — regulators, municipalities, and land-use scholars actively contest the doctrine's scope in nearly every major case, and its boundaries remain a live battleground rather than settled law.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners facing severe diminution and the takings litigation bar sit near the beneficiary end of directionality: the doctrine's existence is what makes their claims or practice viable, and its expansion directly serves their interests. Environmental/land-use regulators and constrained municipalities sit near the target end: they bear the direct fiscal and discretionary cost every time the doctrine is invoked or even threatened, and their exit options are constrained by constitutional supremacy — they cannot simply opt out of the takings framework. Neighboring communities and future regulatory beneficiaries are structurally trapped targets with no seat in the adjudicative process at all; their costs are the most diffuse and least visible, which is precisely why they are least represented in doctrinal debate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Mahon's fear of confiscation-by-regulation) remains genuinely live in some applications — a wetlands designation that destroys 95% of a parcel's value functions economically like a seizure. But the founding_problem_status is authored as 'contested' because substantial corroborating evidence (land-use scholarship, municipal amicus testimony) indicates the doctrine's ad hoc balancing test has become an independent friction generator: litigation increasingly turns on parsing 'investment-backed expectations' and 'character of governmental action' rather than on genuine confiscation concerns. This is not full mandatrophy (the coordination function has not fully atrophied — courts still deny most claims, preserving regulatory space), but the classification as tangled_rope rather than rope captures that a real protective function coexists with a real, asymmetric extraction from regulatory capacity and third-party interests, sustained only by continued active judicial enforcement of the balancing framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_takings_kernel_reading_boundary,
    'Is the ''too far'' regulatory-takings standard a distinct doctrinal reading of the Takings Clause, or is it better understood as the general (default) rule from which the categorical and physical-appropriation readings carve out exceptions?',
    'Trace the doctrinal history: Mahon (1922) established the ''too far'' principle first, before Loretto (1982) carved out the physical-occupation per se rule and Lucas (1992) carved out the total-wipeout categorical rule. If the exceptions were later carve-outs from a pre-existing general rule, the regulatory-takings reading may be structurally prior rather than a co-equal sibling.',
    'If this reading is the historical default and the other two are later exceptions, then the network relationship should be read as this constraint being upstream of the categorical and physical-appropriation readings rather than a parallel sibling — this affects how contamination propagation should flow between the family members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_takings_kernel_reading_boundary, conceptual, 'Whether this reading is a co-equal sibling or the historically prior general rule from which siblings carve exceptions.').

omega_variable(
    chilling_effect_magnitude,
    'How large is the actual chilling effect on regulatory ambition — do regulators demonstrably decline to enact protective regulations because of anticipated takings liability, or is this effect asserted more often than measured?',
    'Empirical study comparing regulatory activity (wetlands designations, historic district creation, downzoning) in jurisdictions with varying degrees of state-level takings liability exposure (some states have enacted takings-compensation statutes stricter than the federal constitutional floor) to isolate the doctrine''s causal contribution.',
    'A large, well-documented chilling effect would support the extractiveness score and the tangled_rope classification (real asymmetric cost imposed on future regulatory beneficiaries); a small or unmeasured effect would suggest the ''victim'' class of future_regulatory_beneficiaries is more speculative than structural, pushing the classification toward a cleaner rope (genuine but modest coordination cost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Whether the doctrine''s chilling effect on future regulation is empirically substantiated or largely asserted.').

omega_variable(
    balancing_test_indeterminacy_as_extraction,
    'Is the Penn Central balancing test''s indeterminacy itself a form of extraction (generating litigation rents and unpredictability costs as a byproduct of the doctrine''s structure), or is it an unavoidable feature of any standard sophisticated enough to distinguish genuine confiscation from ordinary regulation?',
    'Compare litigation cost and outcome-predictability metrics under Penn Central against jurisdictions or historical periods using more categorical takings tests, controlling for underlying regulatory complexity.',
    'If indeterminacy is avoidable (a more precise test could achieve the same protective function with lower friction), the theater_ratio and extractiveness attributable to the balancing test itself are overstated as ''necessary coordination cost'' and understated as extraction; if indeterminacy is unavoidable given the diversity of regulatory takings fact patterns, the current metrics may already be well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_indeterminacy_as_extraction, conceptual, 'Whether the balancing test''s unpredictability is inherent to genuine adjudication or an avoidable extraction-generating design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1922, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1922, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(taki_tr_t2015, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(taki_be_t1922, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1922, 0.32).
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1992, 0.5).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(taki_be_t2015, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1922, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1922, 0.3).
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.38).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1992, 0.46).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(taki_su_t2015, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__regulatory_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the natural-language 'Takings Clause boundary' concept: physical_appropriation_reading (narrowest — physical seizure/occupation only, lowest ε), categorical_takings_reading (middle — per se rules for total wipeouts/physical occupation, Penn Central balancing otherwise, moderate ε), and this story, regulatory_takings_reading (broadest — 'too far' diminution alone triggers compensation via ad hoc balancing, highest ε and highest doctrinal indeterminacy). Each is ε-invariant on its own terms; they are linked because judicial and legislative developments in one reading's application directly shift resource availability and litigation incentives in the others — a narrowing of the categorical per se rules pushes more cases into this reading's ad hoc balancing framework, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__regulatory_takings_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
