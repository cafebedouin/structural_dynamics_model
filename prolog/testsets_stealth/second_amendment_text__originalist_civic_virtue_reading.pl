% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Second Amendment — Originalist Civic-Virtue Reading: Citizen-Soldier Capacity Guarantee
 *   domain: constitutional law/political theory/firearms policy
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the kernel second_amendment_text —
 *   the originalist civic-virtue reading — and is a clean, epsilon-invariant
 *   constraint on its own terms: the constitutional guarantee that the
 *   people's citizen-soldier capacity shall not be infringed, with the
 *   founding-era militia understood as the universal armed citizenry. Under
 *   this reading the arrangement reserves armed civic capacity to the people
 *   as a political body and withholds disarmament power from government; the
 *   citizenry qua political community is the beneficiary, and no victim set
 *   is declared because no party is extracted from — the government's bearing
 *   of the limitation is the allocation the arrangement was built to produce,
 *   not a rent collected by anyone. The epsilon referent is the standing
 *   arrangement under contest (the guarantee as it operates today), assessed
 *   by this reading's own lights: modest but real foreclosure of modern
 *   regulatory discretion, priced by this reading as the cost of the civic
 *   function. Sibling readings — collective_security_reading (right
 *   conditioned on organized civic defense; state may regulate) and
 *   individual_right_reading (individual right independent of militia
 *   service; personal self-defense core) — are separate constraints with
 *   their own epsilon values, beneficiary/victim structures, and
 *   classifications; they are linked by network edges, not folded into this
 *   file. Claim/metric independence is preserved: the constraint is CLAIMED
 *   as rope (civic coordination across generations) while the authored
 *   metrics describe a rope whose civic function has substantially attenuated
 *   (theater 0.45) and whose judicial enforcement has recently intensified
 *   (suppression 0.5) — the engine measures that divergence per seat.
 *
 * KEY AGENTS:
 *   - universal_armed_citizenry: primary beneficiary (organized/constrained) — the people as political body whose retained armed civic capacity the guarantee protects
 *   - citizen_soldiers: individual right-bearers (moderate/constrained) — citizens who hold and exercise the protected capacity
 *   - federal_government: bound party (institutional/constrained) — bears the foreclosure of disarmament power; its bearing is the arrangement's designed output
 *   - state_governments: militia organizers (institutional/constrained) — dual-positioned: draw defensive capacity from the armed citizenry while bearing limits on their own regulatory power
 *   - judiciary: agenda setter (institutional/analytical) — fixes what counts as an infringement and which regulations survive
 *   - gun_regulation_coalitions: policy-seeking seat (organized/constrained) — bears the foreclosure of measures that would disarm or fragment the citizenry's capacity
 *   - gun_violence_affected_communities: absent seat (moderate/constrained) — bear the externalities of widespread civilian arms possession with no voice in the conversation that fixes the guarantee's meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.3).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.5).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment — Originalist Civic-Virtue Reading: Citizen-Soldier Capacity Guarantee").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional law/political theory/firearms policy").

domain_priors:requires_active_enforcement(second_amendment_text__originalist_civic_virtue_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '2a80d118-95cb-483e-8068-7e9ee4279496').
narrative_ontology:cs_kernel_codification('2a80d118-95cb-483e-8068-7e9ee4279496', fixed_text).
narrative_ontology:cs_authority_grounding('2a80d118-95cb-483e-8068-7e9ee4279496', lineage).
narrative_ontology:cs_interpretation_layer_present('2a80d118-95cb-483e-8068-7e9ee4279496').
narrative_ontology:cs_reading_relation('2a80d118-95cb-483e-8068-7e9ee4279496', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a80d118-95cb-483e-8068-7e9ee4279496', second_amendment_text__individual_right_reading, influences).
narrative_ontology:cs_axiom('2a80d118-95cb-483e-8068-7e9ee4279496', foundational, free_state_secured_by_armed_citizenry).
narrative_ontology:cs_axiom_status(free_state_secured_by_armed_citizenry, holdable).
narrative_ontology:cs_axiom_grounding('2a80d118-95cb-483e-8068-7e9ee4279496', free_state_secured_by_armed_citizenry, instrumental).
narrative_ontology:cs_axiom('2a80d118-95cb-483e-8068-7e9ee4279496', foundational, citizenship_carries_arms_bearing_duty).
narrative_ontology:cs_axiom_status(citizenship_carries_arms_bearing_duty, holdable).
narrative_ontology:cs_axiom_grounding('2a80d118-95cb-483e-8068-7e9ee4279496', citizenship_carries_arms_bearing_duty, deontological).
narrative_ontology:cs_reference_frame('2a80d118-95cb-483e-8068-7e9ee4279496', founding_era_armed_citizenry).
narrative_ontology:cs_drift_state('2a80d118-95cb-483e-8068-7e9ee4279496', contemporary_standing_military_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2a80d118-95cb-483e-8068-7e9ee4279496', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, universal_armed_citizenry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizen_soldiers).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, gun_regulation_coalitions).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republican_militia_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, anti_standing_army_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The people of the republic as a political body. The constitutional text reserves to them the capacity to keep and bear arms in their collective defensive role and withholds from government the power to disarm them. What flows to them is a retained civic-military capacity; what the tradition asks of them is the duty side — readiness, organization, the civic practice the founding generation assumed would accompany the right. They act through elections, juries, and the militia structure rather than as a single organized actor; they live inside the constitutional order rather than choosing it, and their exit from it is emigration.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, universal_armed_citizenry, beneficiary,
    organized, generational, constrained, national).

% Individual citizens who keep and bear arms as participants in the civic capacity the text protects. They hold the right in its exercise — acquisition, keeping, bearing — and carry the burdens the civic reading attaches to it: training, readiness, responsibility. Their stake is the retained capacity itself. They cannot opt out of the constitutional order, but the arrangement runs in their favor.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizen_soldiers, beneficiary,
    moderate, biographical, constrained, national).

% The national government. The text withdraws from its menu the option of disarming the citizenry and requires its military policy to accommodate an armed populace. What it bears is a standing limitation on its authority over force — a limitation it did not choose and can remove only through Article V amendment, which the founding design made deliberately arduous. Its day-to-day exit is conformity: shaping policy to fit inside the retained capacity's line.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_government, payer,
    institutional, generational, constrained, national).

% The states. They organize and train the militia and have historically drawn defensive capacity from the armed citizenry whose existence the guarantee preserves — the substrate is theirs to muster but no longer theirs to disarm. Since incorporation they also bear limits on their own police power wherever a measure would break the protected capacity. They benefit from what they can no longer fully control.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_governments, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, state_governments, payer).

% The federal courts, headed by the Supreme Court. They decide what counts as an infringement of the protected capacity, which regulations survive, and what founding-era meaning requires of the present. The arrangement's operative content at any moment is substantially what they hold it to be. They sit outside the benefit and burden flows: nothing is collected to them, nothing is taken from them; they administer the text.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Legislative majorities, advocacy organizations, and public-health institutions that seek to restrict or condition civilian arms possession. Every measure that would disarm the citizenry or break its retained capacity is withdrawn from their reach by the guarantee; their policy options are bounded by whatever line the courts draw around the protected capacity. Their exits are measures that fit inside the line, constitutional amendment, and persuading the courts to move the line.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, gun_regulation_coalitions, payer,
    organized, biographical, constrained, national).

% Communities that bear the externalities of widespread civilian arms possession — homicide exposure, suicide, mass-casualty risk. They hold no seat in the constitutional conversation that fixes what the text protects; their interests reach the courts only through litigants carrying other parties' banners, and they register in the arrangement only insofar as regulation can be fitted inside the protected capacity's line. Nothing in the arrangement's declared structure accounts for them.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, gun_violence_affected_communities, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__originalist_civic_virtue_reading, universal_armed_citizenry).
narrative_ontology:fixing_cost_class(second_amendment_text__originalist_civic_virtue_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational commitment problem of republican defense: every government has a standing incentive to monopolize force in institutions it controls, and no generation of citizens can individually prevent that consolidation. The guarantee entrenches the armed-citizen arrangement against that incentive — each government is bound by a rule it did not make and cannot cheaply unmake, so the citizenry's defensive capacity survives the ordinary life-cycle of political majorities.
% TRANSFER_FUNCTION: Moves no material good. What moves is the locus of military capacity and the discretion over it: the power to disarm the population is withdrawn from government's menu and the capacity to keep and bear arms is reserved to the people. The flow is a standing reallocation of authority — from governing institutions to the citizenry as a body — enforced by judicial review rather than by any periodic payment.
% ABSENT_VOICES: The founding conversation that fixed this arrangement included none of the populations against whom the armed citizenry was actually deployed — enslaved people, Indigenous nations — and excluded women and the unpropertied from the militia whose universality the reading asserts; their descendants' claims reach the modern conversation only obliquely. In the arrangement's current operation, communities bearing the externalities of widespread civilian arms possession hold no seat: they appear in constitutional litigation only through litigants carrying other parties' banners. Both absences are commentary-grade here — they document where consent was never collected, not a correction to the reading's declared structure.
% DISAPPEARANCE_RATIONALE: If the guarantee vanished overnight, the constraint holding governments back from consolidating force would be gone: nothing in the ordinary incentive structure of governing institutions preserves a citizenry's independent armed capacity once no text entrenches it. Under this reading's own account the citizen-soldier substrate would atrophy within a generation or two as professional institutions absorbed the defensive function entirely, and the constitutional allocation of force — people-retained capacity as a check on institutional monopoly — would collapse into state monopoly. The rearrangement is the reading's core prediction; the sibling readings predict different rearrangements, which is their business.
% FOUNDING_PROBLEM: A republic defended by a standing army is vulnerable to the army's masters: the founding generation's solution was to rest defense on the citizenry in arms — a militia composed of the body of the people — and to entrench that arrangement in a bill of rights so that future governments could not quietly substitute the standing force they would prefer.
% FOUNDING_PROBLEM_CORROBORATION: The historical existence of the problem is corroborated from outside any benefiting party: state declarations of rights (Virginia, Pennsylvania, North Carolina, Massachusetts) paired militia guarantees with explicit anti-standing-army clauses, and independent academic historiography of founding-era republican ideology documents the standing-army fear as a genuine and widespread commitment. The problem's continued liveness is NOT corroborated from outside the benefiting parties: the standing military exists, is accepted, and has never been checked by citizen force; the organized militia was statutorily absorbed into the National Guard; the claim that the retained capacity still performs the defensive-civic function is attested today only by the reading's own adherents. That corroboration asymmetry — solid for the founding, absent for the present — is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.3 is low-moderate by the design of this reading: the guarantee moves no material good to any collecting seat; what it reallocates is discretion over force — withheld from government, retained by the citizenry. Even on the reading's own accounting the modern foreclosure is a genuine cost (Bruen-era striking of regulatory measures), so epsilon is nonzero rather than nominal. Suppression 0.5 is authored as a raw structural property and is deliberately NOT scaled by power or scope — the engine owns any scaling: the guarantee is enforced by judicial review that strikes legislation, an active but bounded coercive force whose targets retain alternatives (regulation inside the historical-tradition line, Article V amendment). Theater 0.45 is the honest center of this story: the civic function the reading names — a universal armed citizenry mustering as the polity's defensive substrate — has been institutionally superseded (National Guard as organized militia, a permanent professional military, a paper 'unorganized militia'), so a substantial share of the guarantee's contemporary operation is civic-virtue performance over an attenuated practice, though the enforcement work is real. Accessibility_collapse 0.4: alternatives were never suppressed — the professional standing military the founding generation feared did not merely persist, it became the dominant defense arrangement; the guarantee coexists with its alternative rather than collapsing it. Resistance 0.6: the guarantee meets sustained, organized opposition — legislative regulation campaigns, public-health institutions, and scholarship contesting the reading's historical premises. The three measurement series share one time grid (1791/1830/1865/1903/1939/1968/2008/2026) so no metric's end-state is backfilled into earlier rows: the theater trajectory (0.05 to 0.55) tracks the civic function's atrophy, the suppression trajectory (0.08 to 0.12 to 0.5) tracks enforcement dormancy and post-2008 judicial revival rather than any change in the text, and extractiveness drifts up modestly as enforced foreclosure of regulatory discretion replaced unenforced paper.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by structure. From the universal_armed_citizenry seat (beneficiary, low directionality) the guarantee is a retained capacity — effective extraction damps toward subsidy. From the federal_government seat the same text is a binding withdrawal of authority — high directionality, and the seat computes as the arrangement's target even though what is taken is discretion, not rent, and no beneficiary seat collects it as gain. From the judiciary seat the guarantee is an interpretive mandate: neither benefit nor burden, but the power to fix the line. The same-level lateral pair is citizen_soldiers (moderate power) against gun_regulation_coalitions (organized power): comparable real-world influence, opposite structural relationships to the protected capacity — one holds it, the other seeks measures that would dissolve it — so their computed classifications diverge despite similar power atoms. The inter-institutional pair is federal_government against state_governments: both institutional, both bound post-incorporation, but the states additionally draw militia capacity from the same armed citizenry whose regulation they can no longer fully control, so their exit options and directionalities differ despite equal formal standing. The excluded seat (gun_violence_affected_communities) registers costs that appear in no beneficiary/victim declaration: the guarantee's beneficiary structure is silent about who bears the externalities of the capacity it protects, which is exactly why the seat is authored as absent rather than as a victim.
 *
 * DIRECTIONALITY LOGIC:
 *   The single beneficiary declaration (universal_armed_citizenry) drives the citizenry seats toward the beneficiary end: the arrangement subsidizes them with a retained capacity, and their 'constrained' exit (they live inside the constitutional order) modulates but does not reverse that. The payer roles drive the bound seats toward the target end: federal_government bears the foreclosure directly; gun_regulation_coalitions bear it derivatively through the policy space the guarantee withdraws. state_governments are genuinely dual — they draw militia capacity from the same armed citizenry whose regulation they can no longer fully control — so their derived directionality should sit mid-range, and the dual role is declared rather than forced into a single position. No victim set is declared, and that absence is structural, not evasive: under this reading there is no seat from which value is extracted for another's collection — the government's foreclosed power is not transferred to a collector but destroyed as an option. If a victim set were declared, the constraint would be a different constraint; that is the sibling readings' work, and the reading_indexical_ambiguity omega carries the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a free polity secured by the citizenry in arms rather than by a standing army — has been institutionally overtaken: the National Guard absorbed the organized militia in 1903, a permanent professional military became the actual defense arrangement, and the 'unorganized militia' survives as a statutory fiction. The mandatrophy question is therefore live inside this reading's own frame: if the mandate is dead, the guarantee persists on inertia plus revived judicial enforcement, and the classification drifts rope toward piton (the theater ratio at 0.45 is already the leading symptom). The story declares the founding problem 'contested' rather than 'dead' because the reading itself is the claim that the mandate lives — that the citizenry's retained capacity still performs the civic function — and that claim is what the corpus is measuring. The classification prevents two mislabelings: reading the constraint as a snare (as its opponents do) requires a victim set and a collecting seat that this reading's structure does not contain; reading it as a mountain (as its strongest adherents sometimes do — a natural right antecedent to government) would require emerges_naturality that a ratified, amendable constitutional text does not have. Rope-with-attenuating-function is the honest authorial claim; the engine's per-seat computation may disagree, and that disagreement is the data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexical_ambiguity,
    'This file is one reading of the kernel second_amendment_text; the corpus''s verdict on THIS file measures this reading''s constraint, not the kernel. Which reading''s structure is being classified, and what would the sibling readings change?',
    'Cross-file comparison of the three sibling stories'' computed types, epsilon values, and beneficiary/victim structures: originalist_civic_virtue_reading (citizenry as beneficiary, no victim set), collective_security_reading (states as organizing beneficiaries with regulatory authority), individual_right_reading (individual bearers as beneficiaries, government as constrained party).',
    'Classification is valid only for this reading''s seat. A sibling reading with a declared victim set or a collecting seat would compute a different type from the same text; no verdict on this file adjudicates the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexical_ambiguity, conceptual, 'Kernel/reading indexicality: this constraint is one reading of the Second Amendment text, not the text itself.').

omega_variable(
    militia_universality_historical_claim,
    'Was the founding-era militia actually the universal armed citizenry this reading''s premise requires — or did it exclude the enslaved, women, and the unpropertied, such that the ''citizenry qua political community'' beneficiary was constitutionally narrower than the reading asserts?',
    'Founding-era state militia statutes and enrollment records: who was enrolled, who was exempted, who was barred. The state militia acts are the direct record of the arrangement the reading claims as universal.',
    'If the militia was substantially exclusive, the declared beneficiary is narrower than the political community and the reading''s coordination story carries an exclusion cost its current metrics do not register; the beneficiary structure of this file would need re-authoring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_universality_historical_claim, empirical, 'Historical universality of the founding militia — the empirical premise under the reading''s beneficiary declaration.').

omega_variable(
    civic_function_survivability,
    'Does the citizen-soldier function survive the militia''s institutional supersession — or is the capacity the guarantee protects now performed by institutions (National Guard, professional military) that the guarantee does not reach, leaving the reading''s coordination claim vestigial?',
    'Institutional mapping of what now performs the defensive-civic function: if the function has fully migrated to bodies the guarantee does not protect, the guarantee''s coordination claim is a performance over an absent practice.',
    'If the function has fully migrated, this file''s rope claim decays toward piton under the reading''s own lights — theater_ratio is already 0.45 and the mandatrophy question turns on exactly this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_function_survivability, empirical, 'Whether the protected civic function still exists in practice or persists only as interpretation.').

omega_variable(
    regulatory_foreclosure_cost,
    'Is the militia-capacity line narrow enough to permit meaningful regulation inside it, or does the enforced guarantee foreclose the modern regulatory space wholesale — i.e., is the 0.3 extractiveness a bounded design cost or the leading edge of an expanding foreclosure?',
    'Post-Bruen litigation outcomes: catalogue which regulatory measures survive the historical-tradition test and which are struck, and track the boundary''s movement over time.',
    'A widening foreclosure raises the extraction measured at the government and regulation-seeking seats and pushes the computed classification away from rope; a stable narrow line supports the design-cost reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_foreclosure_cost, empirical, 'Boundedness of the guarantee''s foreclosure of regulatory discretion.').

omega_variable(
    standing_army_problem_liveness,
    'Does the founding problem — standing-force tyranny checked by citizen capacity — have a live modern referent, or is the concern historically closed such that the mandate the reading protects is dead?',
    'Comparative and historical analysis: whether citizen-retained armed capacity has ever actually checked institutional force consolidation in the American record, and whether the modern civil-military arrangement generates the risk the founding problem named.',
    'If the mandate is dead, the guarantee persists on inertia and judicial enforcement — the rope-to-piton drift path opens and the ''contested'' founding_problem_status resolves toward ''dead''. If live, this reading''s coordination claim stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_army_problem_liveness, conceptual, 'Liveness of the anti-standing-army mandate — the pivot of the mandatrophy question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1830, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1830, 0.08).
narrative_ontology:measurement(seco_tr_t1865, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(seco_tr_t1903, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1903, 0.3).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1939, 0.4).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1968, 0.55).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.5).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.12).
narrative_ontology:measurement(seco_be_t1830, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1830, 0.12).
narrative_ontology:measurement(seco_be_t1865, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1865, 0.15).
narrative_ontology:measurement(seco_be_t1903, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1903, 0.18).
narrative_ontology:measurement(seco_be_t1939, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1939, 0.2).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1968, 0.2).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.28).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2026, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1791, 0.08).
narrative_ontology:measurement(seco_su_t1830, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1830, 0.1).
narrative_ontology:measurement(seco_su_t1865, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1865, 0.15).
narrative_ontology:measurement(seco_su_t1903, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1903, 0.18).
narrative_ontology:measurement(seco_su_t1939, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1939, 0.22).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1968, 0.12).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(seco_su_t2026, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2026, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment text is one kernel; this file instantiates the originalist_civic_virtue_reading as a clean, epsilon-invariant constraint. The sibling readings instantiate different constraints from the same text: collective_security_reading makes the states' organized defense the reference arrangement and authorizes regulation (different beneficiary set, plausibly a victim set among disarmed individuals); individual_right_reading makes personal self-defense the protected core (individual bearers as beneficiaries, government as constrained party, no civic-function referent). The epsilon values differ across the family because the referent arrangements differ — this reading's epsilon prices the foreclosure of regulatory discretion against a retained civic function; the individual reading's prices it against personal defense; the collective reading's prices the regulation itself. Coupling direction: the founding-era historical record (this reading's terrain) supplies the evidentiary substrate both siblings argue from, so changes in this reading's standing shift the resource base of the other two — which is why the edge to individual_right_reading is declared as influences — even though neither sibling is logically foreclosed by this one. Each family file links its siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
