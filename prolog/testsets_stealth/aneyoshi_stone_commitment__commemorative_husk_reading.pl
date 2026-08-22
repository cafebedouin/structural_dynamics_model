% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Warning Stones — Commemorative Husk Reading
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   After the 1896 and 1933 Sanriku tsunamis, villagers along the Iwate coast
 *   carved stone markers recording the high-water lines and commanding
 *   rebuilders to keep their homes above them. The Aneyoshi tablets are the
 *   most famous. In 2011 the hamlet stood above the inundation line and
 *   survived intact, and the stones were celebrated worldwide as ancestral
 *   wisdom vindicated. THIS STORY INSTANTIATES ONE READING ONLY — the
 *   commemorative_husk_reading of the aneyoshi_stone_commitment kernel. Under
 *   this reading, the standing arrangement under contest is the stone regime
 *   as it actually operated across the interval: a directive whose behavioral
 *   force decayed to zero, sustained by preservation budgets, ceremonies,
 *   signage, and tourism as a memorial artifact. Land-use decisions were made
 *   independently of the stone directive throughout the late period; the 2011
 *   survival is attributed within this reading to luck, terrain economics,
 *   postwar depopulation, and modern seawall placement rather than to
 *   marker-guided siting; the stone functions as a museum piece. ε's referent
 *   is that standing arrangement, assessed by this reading's own lights —
 *   never the live-rule arrangement the sibling reading would endorse. KEY
 *   AGENTS (by structural relationship): - aneyoshi_descendant_households:
 *   Primary payer (moderate/constrained) — bears residual inundation risk and
 *   observance labor with no operative rule attached -
 *   iwate_municipal_government: Agenda-setter (institutional/mobile) —
 *   administers the memorial regime; runs land use through separate channels
 *   - heritage_tourism_sector: Incidental beneficiary (organized/arbitrage) —
 *   monetizes the stones' fame - disaster_science_community: Excluded
 *   technical voice (organized/mobile) — objects from outside the memorial
 *   forum - temporal_institutional_analyst: Analytical observer
 *   (analytical/analytical) — sees the full decay structure
 *
 * KEY AGENTS:
 *   - aneyoshi_descendant_households: Primary payer (moderate/constrained) — bears residual inundation risk and observance labor; siting decisions run through municipal and economic channels, not the markers
 *   - iwate_municipal_government: Agenda-setter with secondary beneficiary position (institutional/mobile) — administers preservation and ceremony, harvests cultural capital, bears none of the risk
 *   - heritage_tourism_sector: Incidental beneficiary (organized/arbitrage) — collects visitation value from the stones' fame, indifferent to whether the directive binds anyone
 *   - disaster_science_community: Excluded (organized/mobile) — would argue symbolic observance displaces functional mitigation; absent from the memorial forum
 *   - temporal_institutional_analyst: Observer (analytical/analytical) — analytical seat over the 78-year decay arc
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.72).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.14).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.86).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.86).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Warning Stones — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '71f2d0e9-7023-4cfa-9945-5c28a892920e').
narrative_ontology:cs_kernel_codification('71f2d0e9-7023-4cfa-9945-5c28a892920e', fixed_text).
narrative_ontology:cs_authority_grounding('71f2d0e9-7023-4cfa-9945-5c28a892920e', lineage).
narrative_ontology:cs_interpretation_layer_present('71f2d0e9-7023-4cfa-9945-5c28a892920e').
narrative_ontology:cs_reading_relation('71f2d0e9-7023-4cfa-9945-5c28a892920e', aneyoshi_stone_commitment__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('71f2d0e9-7023-4cfa-9945-5c28a892920e', foundational, directive_authority_fully_decayed).
narrative_ontology:cs_axiom_status(directive_authority_fully_decayed, holdable).
narrative_ontology:cs_axiom_grounding('71f2d0e9-7023-4cfa-9945-5c28a892920e', directive_authority_fully_decayed, empirically_contingent).
narrative_ontology:cs_axiom('71f2d0e9-7023-4cfa-9945-5c28a892920e', secondary, commemoration_is_the_stones_proper_function).
narrative_ontology:cs_axiom_status(commemoration_is_the_stones_proper_function, holdable).
narrative_ontology:cs_axiom_grounding('71f2d0e9-7023-4cfa-9945-5c28a892920e', commemoration_is_the_stones_proper_function, conventional).
narrative_ontology:cs_reference_frame('71f2d0e9-7023-4cfa-9945-5c28a892920e', commemorative_inheritance).
narrative_ontology:cs_drift_state('71f2d0e9-7023-4cfa-9945-5c28a892920e', post_2011_disaster_education_campaigns, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('71f2d0e9-7023-4cfa-9945-5c28a892920e', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_sector).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, iwate_municipal_government).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_descendant_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendant families of the hamlet maintain the stones, sweep the approaches, and attend the memorial observances. Their building and rebuilding decisions are made through municipal zoning, seawall placement, hazard maps, and household economics — not by consulting the carved directives. They carry whatever inundation risk remains on their chosen ground, and moving off ancestral land is costly in a depopulating coastal village where lineage continuity is bound to place.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_descendant_households, payer,
    moderate, generational, constrained, local).

% Administers the preservation budget, heritage designation, and the ceremonial calendar around the stones, and controls the interpretive signage and museum presentation. It sets actual land-use policy through an entirely separate apparatus — zoning ordinances, seawall reconstruction programs, designated danger zones. Officials invoke the stones rhetorically in resilience and disaster-education campaigns. The municipality itself bears essentially none of the inundation risk its rhetoric touches.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, iwate_municipal_government, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, iwate_municipal_government, beneficiary).

% Museums, guides, broadcasters, and tour operators monetize the stones' fame as disaster curiosities and 'warnings from the past' content. Their revenue follows visitor attention, not any protective effect of the stones; if public interest faded they would redirect to other attractions without loss to themselves.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_tourism_sector, beneficiary,
    organized, immediate, arbitrage, regional).

% Hazard researchers and engineers who would insist that memorial observance is no substitute for functional mitigation — enforced setback lines, maintained seawalls, evacuation infrastructure. They publish and advise, but they operate in technical planning channels, not in the memorial forum where the stones' meaning is curated; their objection to the symbolic-preparedness framing never enters that room.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_science_community, excluded,
    organized, generational, mobile, national).

% Analytical seat examining the full 78-year arc of the arrangement: what the directive commanded, what the community actually did with its siting decisions, and what the surviving observance now consists of. Collects nothing and pays nothing; sees the decay structure whole.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, temporal_institutional_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the present arrangement, the stones coordinate memorial observance scheduling and heritage presentation — who tends the site, when ceremonies occur, what visitors are shown. The coordination problem the inscriptions originally addressed (where survivors should rebuild relative to the flood marks) is now routed through seawall engineering, municipal zoning, and national hazard mapping.
% TRANSFER_FUNCTION: Moves upkeep labor and ceremonial attention from descendant households and volunteers to the physical preservation of the stones; moves visitor traffic and narrative material to museums and media; and conveys a sense of inherited protection to residents and the public that is not backed by any operative rule on land use.
% ABSENT_VOICES: The erection-generation elders who carved the directives intended a binding instruction to rebuilders; they are dead, and their intent survives only as ceremonial form. The disaster-science community would object that symbolic observance displaces functional mitigation but sits outside the memorial forum. Younger descendants who out-migrated would question the allocation of care labor to stone upkeep in a dying village; they are absent from the observances.
% DISAPPEARANCE_RATIONALE: If the stones and their observance vanished overnight, no siting decision would change: building locations are set by zoning, seawall geometry, hazard maps, and household economics, none of which consult the markers. Ceremonies would cease, a minor tourist draw would disappear, and the municipality would lose a rhetorical prop — the settlement pattern itself would not rearrange.
% FOUNDING_PROBLEM: The 1896 and 1933 Sanriku tsunamis annihilated coastal hamlets in Iwate; the stones were erected to mark the high-water lines and command survivors to rebuild their homes above them, encoding a flood-memory directive in stone for a population that had just lost its archives and its elders.
% FOUNDING_PROBLEM_CORROBORATION: Meiji- and Shōwa-era damage records, prefectural tsunami inventories, and academic disaster historiography corroborate that the founding problem existed and was severe. That the problem is now dead is attested from outside the beneficiary set by postwar and post-2011 hazard-governance instruments — seawall reconstruction programs, legally designated danger zones, and municipal hazard maps — which show the siting function migrating wholesale into modern institutions.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the arrangement takes real value while delivering none of its nominal good: descendant households supply upkeep labor and ceremonial attention, and the public inherits an assurance ('the ancestors' warnings govern this coast') that corresponds to no operative constraint — an assurance-substitution whose cost is paid in unexamined residual risk. Suppression is low (0.14) and is authored as a raw, unscaled structural property: a husk coerces almost nothing, alternatives are abundant and cheap, and only a thin reputational cost attaches to privately ignoring the stones. Theater_ratio is very high (0.86): plaques, bus tours, ceremonies, and disaster-education rhetoric constitute nearly all current activity around the stones, against approximately zero behavioral constraint. Accessibility_collapse is low (0.20) — once the husk character is understood, the alternatives (zoning, seawalls, hazard maps, evacuation drills) remain fully available; nothing collapses. Resistance is low (0.18): almost no one actively contests a memorial; scholarly discomfort with the mythologization is marginal and channel-separated. The temporal series run on one shared eight-point grid (every tracked metric authored at every point, 1933–2011): extractiveness and theater rise monotonically as modern infrastructure absorbed the directive's function while observance continued, and suppression_requirement falls monotonically — an enforcement-decay trajectory, from a post-disaster norm that carried real obligating force in 1933 to purely voluntary observance by 2011. The trajectory is monotonic decay, not cyclical; no intermittent-reinforcement mechanism is claimed. Receipt surface, authored on its own evidence: gain_flow is 'diffuse' as an affirmative finding — each named seat was checked, and none receives the extracted value; tourism collects willing visitor spending (a benefit, not the extraction), the municipality collects diffuse cultural capital, and the assurance-substitution and risk are borne by households but collected by no one. fixing_cost is 'prohibitive' relative to benefit: physically decommissioning the memorial would be trivially cheap but deliver a negligible benefit (removing a museum piece protects no one), while the fix that matters — recoupling the directive to enforceable land use — is institutionally prohibitive; on the cost-relative-to-benefit reading both routes deter action, which is why the husk persists.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat should compute differently. From the descendant households' position the arrangement is inherited obligation fused with unexamined exposure: they tend the stones, and their constrained exit (relational identity binding lineage to ancestral ground) keeps them near the full-target end of directionality despite their nominal freedom to ignore the markers. From the municipality's position the same arrangement is a low-cost heritage asset and a ready-made rhetoric of resilience — it administers the husk, draws prestige from it, and bears none of its risk. The tourism seat experiences the stones purely as content. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   aneyoshi_descendant_households are the declared victims: they bear the transferred costs (observance labor, assurance without protection, residual risk), and their exit is constrained by lineage-place identity, pushing derived directionality toward the full-target end and amplifying effective extraction. heritage_tourism_sector and iwate_municipal_government are declared beneficiaries: the first collects visitation value with arbitrage-grade exit (nearest the beneficiary end), the second collects administrative prestige and avoided enforcement cost while running the agenda — a dual-positioned seat the derivation handles through its beneficiary declaration plus agenda-setter role. Spatial scope is local, so the engine's scope amplification of effective extraction stays modest; the high χ at the household seat comes from directionality and trapped exit, not scope. Suppression remains unscaled throughout — it is a raw structural input, and here it is genuinely small.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate — command rebuilders above the flood marks — has been fully outlived: its function migrated to seawalls, zoning, and hazard mapping, and the arrangement persists as observance alone. mandatrophy_resolved is declared true. Classifying the husk as a piton prevents two misreadings: the sibling reading's error of counting a museum piece as a live coordination rope (which would credit the stone for 2011 survival and license symbolic preparedness as sufficient), and the opposite error of reading it as a snare (which would require a capturer collecting the extraction — and the receipt surface shows the gains are diffuse; no seat collects). The piton classification locates the truth between: extraction without capture, performance without function, persistence by inertia and sentiment because the administrator could change it but bears none of its cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (commemorative_husk_reading) of the aneyoshi_stone_commitment kernel; what would the sibling reading (behavioral_competence_reading) change structurally, and where exactly is the disagreement located?',
    'The sibling reading is authored as its own constraint story with its own ε, beneficiaries, and claimed type; the disagreement between readings is located in a single structural element — whether building-location decisions across 1933–2011 causally tracked the marker elevations.',
    'If the sibling reading is adopted, ε drops sharply (a live coordination rule with negligible extraction), the victim set empties, and the classification moves toward rope; the entire piton profile of this story is conditional on the husk reading holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one of two declared readings of the stone-commitment kernel; the sibling would invert the extraction profile.').

omega_variable(
    siting_causality_attribution,
    'Was the hamlet''s survival in 2011 attributable to marker-guided siting decisions, or to independent factors — postwar depopulation, terrain economics, seawall placement, municipal designation?',
    'Archival construction permits and household relocation histories cross-referenced against marker elevations and dates: if siting decisions demonstrably consulted the markers after the directive''s normative era ended, the behavioral_competence_reading gains support; if siting tracks economics and infrastructure instead, the husk reading holds.',
    'Resolves which reading of the kernel is empirically true; flips ε between roughly 0.15 and 0.72 and swings the classification between rope and piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(siting_causality_attribution, empirical, 'The causal-attribution question on which the two kernel readings divide.').

omega_variable(
    false_assurance_displacement,
    'How much does memorial observance around the stones measurably displace independent risk assessment and functional preparedness among residents and visitors?',
    'Behavioral studies of preparedness in communities with prominent heritage-memorial framing versus comparable communities without it: drill participation, evacuation-route knowledge, insurance uptake, siting inquiries.',
    'Scales the assurance-substitution component of ε: if displacement is large, the husk is actively harmful rather than merely inert, pushing effective extraction up at the household seat; if negligible, part of the measured extraction is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_assurance_displacement, empirical, 'Magnitude of the symbolic-for-functional substitution effect underlying the high ε claim.').

omega_variable(
    commemoration_vs_directive_referent,
    'Is ''the constraint'' the decayed directive (a commitment that once bound and no longer does) or the live memorial regime (an arrangement that currently organizes observance and heritage presentation)?',
    'Specify which arrangement the classifier is evaluating: the directive-framing asks what the stones command; the memorial-regime framing asks what the preservation-and-ceremony apparatus does. Signals guiding the choice here: the story''s ε referent is the standing arrangement under contest, which post-2011 is overwhelmingly the memorial regime plus residual rhetorical invocations of the directive.',
    'Under a pure memorial-regime framing the arrangement looks like a low-stakes heritage rope with negligible ε; under the directive-framing it is a decayed commitment whose husk status is the entire finding. This story adopts the directive-framing because the kernel under contest is the commitment, and the memorial regime is what the commitment decayed into.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commemoration_vs_directive_referent, conceptual, 'CS-framing under-determination: which layer of the arrangement is the constraint proper.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.12).
narrative_ontology:measurement(aney_tr_t1945, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1960, 0.33).
narrative_ontology:measurement(aney_tr_t1968, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1968, 0.44).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1980, 0.58).
narrative_ontology:measurement(aney_tr_t1995, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1995, 0.7).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2005, 0.8).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.86).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.22).
narrative_ontology:measurement(aney_be_t1945, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1945, 0.28).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(aney_be_t1968, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1968, 0.47).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1980, 0.56).
narrative_ontology:measurement(aney_be_t1995, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1933, 0.55).
narrative_ontology:measurement(aney_su_t1945, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(aney_su_t1968, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1968, 0.34).
narrative_ontology:measurement(aney_su_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(aney_su_t1995, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement(aney_su_t2005, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2005, 0.17).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2011, 0.14).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Aneyoshi stone commitment.' The label conflates two structurally distinct claims: (1) behavioral_competence_reading — the stone as a live land-use rule with operational force across 78 years, low ε, rope-flavored; (2) commemorative_husk_reading (this file) — the stone as a memorial artifact with zero behavioral constraint, high ε from assurance-substitution and observance overhead, piton-flavored. The ε values differ by a wide margin because the referents differ in kind: a functioning directive versus its ceremonial residue. The upstream story (higher folk confidence, media-celebrated) is the sibling; this reading contests it and cites it as the claim whose acceptance lets symbolic preparedness pass for protection. Linked via network.affects_constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
