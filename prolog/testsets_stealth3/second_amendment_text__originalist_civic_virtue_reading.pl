% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment — Originalist Civic-Virtue Reading (Universal Armed Citizenry)
 *   domain: constitutional law/political theory/firearms policy
 *
 * SUMMARY:
 *   The constraint authored here is the originalist civic-virtue reading of
 *   the Second Amendment kernel: the founding-era militia understood as the
 *   universal armed citizenry — the enrolled body of the people itself, not a
 *   select organized force — and the amendment's guarantee as protecting the
 *   citizen-soldier capacity that constitutes that body. On this reading the
 *   beneficiary is the political community as a whole: the arrangement
 *   preserves a defense establishment composed of the people, forecloses
 *   government disarmament of the civic body, and ties the right's legitimacy
 *   to its civic-republican function rather than to personal self-defense or
 *   to state regulatory purposes. No victim set is declared, because within
 *   this reading's frame no identifiable group is extracted from; the seat
 *   that bears the operating costs (governments attempting regulation) is
 *   assigned the position of appropriately limited governor, not victim —
 *   authoring it as a victim would import sibling-frame content into a
 *   reading that does not contain it. The claim/metric gap is deliberate: the
 *   reading is CLAIMED as rope (genuine coordination of common defense
 *   without a standing army) while the authored metrics record substantial
 *   theatrical maintenance, rising late-interval enforcement intensity, and
 *   persistent resistance. This story is one member of a three-story
 *   constraint family decomposing the colloquial label 'the Second
 *   Amendment'; see network.dual_formulation_note and the kernel-reading
 *   omega.
 *
 * KEY AGENTS:
 *   - - citizenry_qua_political_community: Primary beneficiary (organized/generational, constrained exit) — collects the preserved arms-bearing capacity of the civic body
 *   - - arms_owning_citizen_soldiers: Operational beneficiary with duty-side cost exposure (organized/generational, identity_locked exit) — the militia's material; capacity fused with civic membership
 *   - - federal_judicial_interpreters: Agenda-setter (institutional/generational, constrained exit) — administers the fixed kernel through the lineage-interpretation layer
 *   - - state_regulatory_authorities: Payer seat (institutional/biographical, constrained exit) — bears foreclosed policy optionality and litigation costs; positioned by the reading as legitimately limited, not victimized
 *   - - founding_excluded_populations: Excluded voice (powerless/biographical, trapped exit) — outside the founding 'universal' militia, whose objection to the arrangement was total
 *   - - civic_republican_scholars: Analytical observer (analytical/civilizational, analytical exit) — reconstructs and stress-tests the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.26).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.55).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment — Originalist Civic-Virtue Reading (Universal Armed Citizenry)").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional law/political theory/firearms policy").

domain_priors:requires_active_enforcement(second_amendment_text__originalist_civic_virtue_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '832e8999-3ec7-4e49-8390-b1f9423fc135').
narrative_ontology:cs_kernel_codification('832e8999-3ec7-4e49-8390-b1f9423fc135', fixed_text).
narrative_ontology:cs_authority_grounding('832e8999-3ec7-4e49-8390-b1f9423fc135', lineage).
narrative_ontology:cs_interpretation_layer_present('832e8999-3ec7-4e49-8390-b1f9423fc135').
narrative_ontology:cs_reading_relation('832e8999-3ec7-4e49-8390-b1f9423fc135', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('832e8999-3ec7-4e49-8390-b1f9423fc135', second_amendment_text__individual_right_reading, influences).
narrative_ontology:cs_axiom('832e8999-3ec7-4e49-8390-b1f9423fc135', foundational, militia_is_the_whole_enrolled_people).
narrative_ontology:cs_axiom_status(militia_is_the_whole_enrolled_people, holdable).
narrative_ontology:cs_axiom_grounding('832e8999-3ec7-4e49-8390-b1f9423fc135', militia_is_the_whole_enrolled_people, empirically_contingent).
narrative_ontology:cs_axiom('832e8999-3ec7-4e49-8390-b1f9423fc135', foundational, arms_bearing_is_necessary_to_free_state_security).
narrative_ontology:cs_axiom_status(arms_bearing_is_necessary_to_free_state_security, holdable).
narrative_ontology:cs_axiom_grounding('832e8999-3ec7-4e49-8390-b1f9423fc135', arms_bearing_is_necessary_to_free_state_security, instrumental).
narrative_ontology:cs_reference_frame('832e8999-3ec7-4e49-8390-b1f9423fc135', founding_era_universal_militia_order).
narrative_ontology:cs_drift_state('832e8999-3ec7-4e49-8390-b1f9423fc135', contemporary_professional_military_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('832e8999-3ec7-4e49-8390-b1f9423fc135', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizenry_qua_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, arms_owning_citizen_soldiers).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, arms_owning_citizen_soldiers).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, state_regulatory_authorities).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republican_militia_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, anti_standing_army_principle).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, universal_enrollment_historiography).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The enrolled political community as a whole. The arrangement preserves its collective arms-bearing capacity as a standing feature of the polity: the community retains, in principle, a defense establishment composed of itself rather than a separate professional force. What flows to it is retained civic capacity and a constitutional guarantee that no government may disarm the body politic. On the reading's account of the founding arrangement, the counterpart flowing from it is duty — service, training, muster. Exit is nominal only: the community cannot resign from its own constitutional order short of formal amendment or dissolution.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizenry_qua_political_community, beneficiary,
    organized, generational, constrained, national).

% Individuals who hold arms as participants in the civic body rather than as private consumers. On this reading they are the militia's material: their arms-bearing is intelligible as citizen-soldier capacity, carrying the expectation of availability for common defense. They collect the guaranteed capacity; on the duty side they bear the obligations the capacity exists to serve — training, readiness, potential call-up. Leaving the position means leaving the civic identity itself: the citizen-soldier role is fused with membership in the political community, not an interest that can be dropped.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, arms_owning_citizen_soldiers, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, arms_owning_citizen_soldiers, payer).

% The courts and the interpretive tradition that administer the fixed text: they adjudicate what counts as infringement, resolve collisions between the guarantee and legislative regulation, and thereby set the day-to-day terms of the arrangement without having originated it. They inherit a fixed kernel and a transmission tradition (originalist hermeneutics) and absorb drift through interpretation rather than revision. Their exit is bounded by office: judges cannot step outside the text they are sworn to uphold, and the interpretive method is itself part of what is administered.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_judicial_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% State legislatures and agencies that attempt to regulate civilian arms — licensing, registration, carry restrictions — and find their measures challenged, reversed, or pre-empted by the constitutional guarantee as interpreted. What flows from them is foreclosed policy optionality plus the recurring litigation and reversal costs of attempted regulation; nothing comparable flows back. Within this reading's frame they are positioned as appropriately limited governors rather than as an extracted victim class, though they are the seats that bear the constraint's operating costs. Exit runs through Article V, which no single state can traverse alone.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_regulatory_authorities, payer,
    institutional, biographical, constrained, regional).

% Those outside the founding-era 'universal' militia — the enslaved and the otherwise disenfranchised — for whom the armed-citizen arrangement was not protection but menace: the capacity being guaranteed was in practice deployed to police them, and they had no voice in the compact that guaranteed it. Their objection to the arrangement would have been total. Their descendants sit inside the political community today, but the historical exclusion marks the boundary of the claim that the militia was ever truly universal.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, founding_excluded_populations, excluded,
    powerless, biographical, trapped, national).

% Political theorists and constitutional historians working in the civic-republican tradition: they reconstruct the founding understanding of the militia, test the universality claim against the archival record, and articulate this reading's case in scholarly form. They collect no rents from the arrangement and bear none of its operating costs; their stake is analytic — the survival and plausibility of the interpretive tradition they work within.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, civic_republican_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__originalist_civic_virtue_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_text__originalist_civic_virtue_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates common defense without a standing professional army: by distributing arms-bearing capacity across the citizen body, the polity obtains a defense establishment that is the people themselves, eliminating the distinct professional force the founding generation feared as an instrument of tyranny.
% TRANSFER_FUNCTION: Moves the cost and honor of military defense from a professional soldiery onto the entire enrolled citizen body; and, in its contemporary operation, transfers regulatory discretion away from elected governments toward an entrenched constitutional entitlement held by the people at large.
% ABSENT_VOICES: At the founding: the enslaved and otherwise disenfranchised, excluded from the supposedly universal militia, whose objection to the arrangement was total and who had no seat in the compact that made it. Today: the communities that bear the public-safety costs of widespread civilian armament, and the public-health research community; both stand outside the civic-virtue conversation, which is conducted among jurists, historians, and rights advocates.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, regulation legislation currently foreclosed would pass immediately, two centuries of militia jurisprudence built on the text would lose its anchor, the civic-republican interpretive tradition would lose its constitutional object, and the political coalition organized around the entitlement would lose its legal keystone. Courts, legislatures, and a mass membership movement all arrange themselves around the text, so removal forces extensive rearrangement.
% FOUNDING_PROBLEM: How a free polity provides for its common defense without maintaining a standing professional army — the founding generation's inherited fear that a permanent military establishment becomes an instrument of executive domination, answered by making the enrolled citizen body itself the defense establishment.
% FOUNDING_PROBLEM_CORROBORATION: Militia-system historians, working outside the beneficiary set, document that compulsory universal enrollment and muster had collapsed by the 1840s and that major mobilizations (the Civil War above all) relied on volunteers rather than the enrolled militia — attesting that the founding mechanism failed early as an operating defense system. Comparative constitutional scholars attest that no peer polity relies on an armed citizenry for national defense. Civic-virtue adherents attest continued relevance of the capacity. There is no consensus attestation from outside the beneficiary set; the strongest outside attestation is of substantial obsolescence of the original mechanism, with the entitlement's persistence explained by textual entrenchment.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.26 at interval end) because the standing arrangement, assessed by this reading's own lights, is primarily protective — a subsidy to civic capacity rather than a rent-collection mechanism. The residual reflects real operation: the constraint functions as a veto that extracts policy optionality from governments attempting regulation and leaves diffuse public-safety costs unpriced, costs even the civic frame partially concedes (the founding text itself speaks of a 'well regulated' militia). Suppression is a raw structural property, unscaled by power or scope: 0.55 records the constitutional foreclosure of an entire regulatory alternative space — comprehensive licensing and registration regimes that peer democracies operate are blocked domestically by interpretive veto, not by participant preference. Theater ratio 0.52 is the honest midpoint of a documented history: the function was real at t=0 (live musters, actual defense role), decayed through the antebellum period (musters degenerating into social occasions), peaked theatrically around t=90 after the universal-militia system had effectively collapsed and the text persisted by inertia and pageantry, dipped as regulation battles gave the text renewed legal stakes, and rose again as contemporary invocation became heavily symbolic. Accessibility collapse 0.38: the professional-standing-army alternative was fully realized long ago and dominates globally; the domestic disarmament alternative is foreclosed but visibly available abroad; alternatives narrow but do not vanish once the constraint is understood. Resistance 0.6 records sustained, organized opposition to the constraint's practical operation (regulatory movements, public-health advocacy, litigating jurisdictions). The measurement series run on one shared time grid (years since ratification: 0, 45, 90, 135, 180, 235) with every tracked metric authored at every point; the non-monotonic shapes are substantive — the extractiveness dip at t=180 reflects a window in which major regulation actually passed, and the suppression dip at t=135 reflects the era in which judicial interpretation read the amendment as supporting regulation rather than blocking it, before the late-interval enforcement intensification.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the citizen-body seat the arrangement is insurance: a guarantee that the polity's defensive capacity cannot be confiscated, experienced as civic inheritance. From the state-regulator seat the identical structure is a recurring veto over democratically adopted policy, experienced as foreclosed optionality paid for in litigation and reversal. The interpreter seat experiences administration, not ownership: it sets daily terms of a text it did not write and cannot exit. Identity-lock dynamics concentrate in the arms_owning_citizen_soldiers seat: the fusion is institutional-civic rather than consumer-grade — the citizen-soldier role is constituted through membership in the political community itself, so exiting the arrangement would require exiting the identity the reading says the arrangement protects; if that civic fusion broke (if arms-holding were reframed as private preference rather than civic capacity), the seat's exit options would loosen toward 'mobile' and the duty-side expectations would detach from the entitlement. The excluded seat is the perspective the reading's own historiography must confront: the 'universality' it celebrates was bounded, and those outside the boundary experienced the guaranteed capacity aimed at them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration (citizenry_qua_political_community) derives low directionality for the civic-body seat and, through its operational membership, for the arms-owning citizen-soldier seat — the arrangement subsidizes rather than extracts from them, with the secondary payer role recording the duty-side cost exposure that the civic function nominally requires. The state_regulatory_authorities seat carries role payer on the stakeholder surface but is deliberately NOT entered in base_properties.victims: the reading assigns it the position of legitimately limited governor, and promoting it to victim would smuggle in the adversarial frame of sibling readings. Its elevated cost-bearing is therefore recorded structurally (payer role, constrained exit, Article V lock) without converting the story into a victim-declaring type. No directionality overrides are used: the derivation chain from the beneficiary declaration plus exit options produces the correct relationship for every seat, and an override keyed to the institutional power atom would wrongly move the judicial interpreters, who sit on the administrative rather than the cost-bearing side of the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming rope guards the genuine coordination core from being misread as pure extraction: the founding arrangement really did solve a defense-provisioning problem (common defense without a standing army) and really did make participants net beneficiaries in its own frame. Equally, the analysis refuses the reverse error: the mandate question — whether the citizen-soldier function is live or vestigial — is routed to an omega variable rather than asserted, so a future piton flip would be data-driven rather than presupposed by this story. The founding problem is authored as contested, not dead: the standing-army fear that motivated the arrangement is partially obsolete (a professional military exists and dwarfs any citizen muster), but parties dispute whether the capacity remains load-bearing. Because status is contested rather than dead, the dead-mandate-plus-world_rearranges mismatch does not fire automatically; the zombie question stays open for the corpus to settle. Mandatrophy_resolved is deliberately not declared: the mandate's status is precisely what is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation_delta,
    'This constraint is the originalist_civic_virtue_reading of kernel second_amendment_text — how would instantiating a sibling reading change the structural data?',
    'Authoring the sibling stories and comparing compiled structure: collective_security_reading would add the state as coordinating beneficiary and regulator, introduce a regulated-persons victim set, and raise enforcement intensity; individual_right_reading would shift the beneficiary to private persons, recast the core protected activity as personal self-defense, and introduce crime-exposure cost-bearers this reading does not declare.',
    'Beneficiary identity, victim set, and extractiveness all move across readings of the same kernel text; cross-reading comparison is valid only across the separately authored, network-linked stories, never within one story hedged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation_delta, conceptual, 'Committer structure: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    citizen_soldier_function_liveness,
    'Is the citizen-soldier capacity this reading protects a live coordination input, or a vestigial function maintained by constitutional inertia and civic symbolism?',
    'Defense-readiness analysis: whether civilian arms-bearing capacity measurably contributes to mobilization, recruitment pipelines, territorial or improvised defense, or deterrence (including comparable arrangements abroad), versus existing only as constitutional text, litigation posture, and civic rhetoric.',
    'If vestigial, the authored theater ratio understates decay and a piton reclassification becomes the honest terminal state for this reading''s arrangement; if live, the rope claim stands and the duty-side expectations attached to the capacity revive with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_soldier_function_liveness, empirical, 'Whether the protected function is operational or atrophied — the rope-versus-piton hinge.').

omega_variable(
    universality_of_founding_militia,
    'Was the founding-era militia actually universal, or bounded by property, race, and gender such that ''citizenry qua political community'' named a far narrower body than the reading assumes?',
    'Archival reconstruction of enrollment requirements, exemption patterns, and exclusion effects across state militia codes, roughly 1776-1830, and of who the guaranteed capacity was deployed against.',
    'If bounded, the beneficiary declaration narrows, the vindicated civic function covered fewer persons than claimed, and the founding_excluded_populations seat moves from historical footnote to structural member of the arrangement — raising the reading''s implicit extraction and complicating its beneficiary-centered classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_of_founding_militia, empirical, 'Whether the ''universal armed citizenry'' premise survives contact with the enrollment record.').

omega_variable(
    naturality_vs_construction,
    'Does the constraint reflect a natural civic necessity — as the reading''s tradition asserts when it grounds arms-bearing in the security of a free state — or a constructed constitutional choice that persists chiefly because it was written into a hard-to-amend text?',
    'Comparative constitutional analysis and counterfactual institutional design: whether polities lacking the guarantee converge on equivalent arrangements, and whether the arrangement would survive hypothetical re-authoring under contemporary conditions.',
    'Naturality would push the arrangement toward mountain-like immunity from revision; construction keeps it in revisable coordination territory and explains its persistence by Article V entrenchment and interpretive lineage rather than by necessity — changing which remedies are even conceptually available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_construction, conceptual, 'The natural-law versus constructed-constraint ambiguity the reading''s own rhetoric generates.').

omega_variable(
    capacity_duty_coupling,
    'Does protecting citizen-soldier capacity entail enforceable civic duties — training, readiness, service — or is the capacity protected without its obligation?',
    'Analysis of the reading''s own texts and of the historical militia acts that coupled entitlement with obligation, together with the legislative fate of modern attempts to re-couple them; the founding template coupled a guaranteed capacity to mandated muster.',
    'If duties are entailed, the arms_owning_citizen_soldiers seat''s payer side becomes primary, extraction rises above the protective floor, and the arrangement stops resembling pure subsidy; if decoupled, capacity-without-obligation is difficult to distinguish from a private entitlement wearing civic dress, weakening this reading''s distinction from the individual-right sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_duty_coupling, conceptual, 'Whether the civic function carries its obligation with it, or the right floats free of the duty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t45, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 45, 0.24).
narrative_ontology:measurement_basis(seco_tr_t45, observed).
narrative_ontology:measurement(seco_tr_t90, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 90, 0.5).
narrative_ontology:measurement_basis(seco_tr_t90, observed).
narrative_ontology:measurement(seco_tr_t135, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 135, 0.46).
narrative_ontology:measurement_basis(seco_tr_t135, observed).
narrative_ontology:measurement(seco_tr_t180, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 180, 0.42).
narrative_ontology:measurement_basis(seco_tr_t180, observed).
narrative_ontology:measurement(seco_tr_t235, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 235, 0.52).
narrative_ontology:measurement_basis(seco_tr_t235, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t45, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 45, 0.12).
narrative_ontology:measurement_basis(seco_be_t45, observed).
narrative_ontology:measurement(seco_be_t90, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 90, 0.18).
narrative_ontology:measurement_basis(seco_be_t90, observed).
narrative_ontology:measurement(seco_be_t135, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 135, 0.22).
narrative_ontology:measurement_basis(seco_be_t135, observed).
narrative_ontology:measurement(seco_be_t180, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 180, 0.21).
narrative_ontology:measurement_basis(seco_be_t180, observed).
narrative_ontology:measurement(seco_be_t235, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 235, 0.26).
narrative_ontology:measurement_basis(seco_be_t235, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t45, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 45, 0.17).
narrative_ontology:measurement_basis(seco_su_t45, observed).
narrative_ontology:measurement(seco_su_t90, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 90, 0.26).
narrative_ontology:measurement_basis(seco_su_t90, observed).
narrative_ontology:measurement(seco_su_t135, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 135, 0.21).
narrative_ontology:measurement_basis(seco_su_t135, observed).
narrative_ontology:measurement(seco_su_t180, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 180, 0.31).
narrative_ontology:measurement_basis(seco_su_t180, observed).
narrative_ontology:measurement(seco_su_t235, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 235, 0.55).
narrative_ontology:measurement_basis(seco_su_t235, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, resource_allocation).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Second Amendment' decomposes into three structurally distinct constraints sharing one fixed text as kernel. This story instantiates the originalist civic-virtue reading: beneficiary is the citizenry qua political community, no victim set is declared, and extractiveness is assessed for the civic-function arrangement by that reading's own lights (0.26). The collective-security sibling adds the organized militia/state as coordinating beneficiary, introduces a regulated-persons victim set, and carries heavier enforcement; the individual-right sibling shifts the beneficiary to private persons, recasts the core protected activity as personal self-defense, and carries a crime-exposure cost structure this reading does not author. The three share an evidentiary substrate — founding-era militia historiography — and this reading supplies the universality historiography on which the individual-right sibling draws (relation: influences), while remaining structurally distinct enough that merging them into one observable-dependent story would violate epsilon-invariance. Each reading yields its own epsilon, beneficiary structure, and classification, so they are authored as separate linked files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
