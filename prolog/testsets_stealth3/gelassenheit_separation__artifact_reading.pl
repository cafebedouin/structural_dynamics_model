% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Visible-Marker Separation Regime (Artifact Reading of the Gelassenheit Kernel)
 *   domain: religious/technological/commitment-systems
 *
 * SUMMARY:
 *   Old Order Anabaptist districts govern member conduct through the Ordnung,
 *   an administered body of rules fixing dress, technology, and household
 *   practice. This story instantiates ONE reading of the
 *   gelassenheit_separation kernel — the artifact reading: separation from
 *   English society is defined as VISIBLE DISTINCTION, and the operative test
 *   for any technology or garment is whether it resembles a worldly artifact,
 *   regardless of function. A rooftop solar array that would leave a
 *   household fully off-grid is forbidden because mounted panels look like
 *   English technology; synthetic fabric is forbidden because it looks like
 *   English cloth even when sewn into prescribed patterns at home. The
 *   sibling readings — principle_reading (avoid structural entanglement;
 *   functionally isolated technology acceptable) and consequence_reading
 *   (evaluate technology by its effect on visiting, mutual aid, and
 *   geographic rootedness) — are separate constraints with their own epsilon,
 *   victim sets, and failure modes; they are not folded into this story. The
 *   epsilon referent here is the standing artifact-marker arrangement as it
 *   operates, assessed through this reading's own lights: the reading counts
 *   obedience-cost as offering, but its own binding criterion is
 *   function-blind, so a large share of the cost members bear purchases
 *   nothing the reading itself can name except visibility. Claim and metrics
 *   are authored independently: the claimed type states my structural belief;
 *   the metrics state the operation as I read the record.
 *
 * KEY AGENTS:
 *   - ordnung_ministry: agenda-setter (institutional / identity_locked) — fixes the forbidden-artifact list, hears confessions, imposes shunning; collects interpretive authority over what separation requires
 *   - rank_and_file_members: primary target (powerless / identity_locked) — bears compliance costs and surrenders private technology judgment; also incidental beneficiary of the boundary the markers maintain
 *   - rumspringa_youth: pre-commitment window (powerless / mobile) — samples English life before baptism; consent is collected at maximum social dependence
 *   - dissenting_households: resistant target (moderate / constrained) — covert noncompliance, informal petitions, occasional factional schism threats
 *   - offgrid_technology_suppliers: excluded sellers (powerful / arbitrage) — barred from the community's business even where their products would deepen the buyer's isolation from worldly systems
 *   - looser_ordnung_districts: excluded counter-model (institutional / mobile) — same tradition, different reading; demonstrate the suppressed alternative from inside
 *   - religious_studies_observer: analytical observer (analytical / analytical) — documents Ordnung change, shunning practice, and retention across districts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.74).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.88).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Visible-Marker Separation Regime (Artifact Reading of the Gelassenheit Kernel)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technological/commitment-systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '20f9c311-958c-480a-8b02-63c28a2c62e5').
narrative_ontology:cs_kernel_codification('20f9c311-958c-480a-8b02-63c28a2c62e5', distributed).
narrative_ontology:cs_authority_grounding('20f9c311-958c-480a-8b02-63c28a2c62e5', lineage).
narrative_ontology:cs_interpretation_layer_present('20f9c311-958c-480a-8b02-63c28a2c62e5').
narrative_ontology:cs_reading_relation('20f9c311-958c-480a-8b02-63c28a2c62e5', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('20f9c311-958c-480a-8b02-63c28a2c62e5', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('20f9c311-958c-480a-8b02-63c28a2c62e5', foundational, separation_requires_visible_markers).
narrative_ontology:cs_axiom_status(separation_requires_visible_markers, holdable).
narrative_ontology:cs_axiom_grounding('20f9c311-958c-480a-8b02-63c28a2c62e5', separation_requires_visible_markers, theological).
narrative_ontology:cs_axiom('20f9c311-958c-480a-8b02-63c28a2c62e5', foundational, worldly_resemblance_forbids_regardless_of_function).
narrative_ontology:cs_axiom_status(worldly_resemblance_forbids_regardless_of_function, holdable).
narrative_ontology:cs_axiom_grounding('20f9c311-958c-480a-8b02-63c28a2c62e5', worldly_resemblance_forbids_regardless_of_function, conventional).
narrative_ontology:cs_reference_frame('20f9c311-958c-480a-8b02-63c28a2c62e5', visible_marker_separation_regime).
narrative_ontology:cs_drift_state('20f9c311-958c-480a-8b02-63c28a2c62e5', contemporary_offgrid_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20f9c311-958c-480a-8b02-63c28a2c62e5', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, ordnung_ministry).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, rank_and_file_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, rank_and_file_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, rumspringa_youth).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, dissenting_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% District bishops and ministers who administer the Ordnung: they deliberate each new artifact case (a mounted solar panel, a polyester blend, a powered buggy light), fix the district's forbidden list, hear confessions, and impose ban and shunning for defiance. The offices are unpaid and drawn from the membership, but the office carries the community's interpretive authority: what separation requires is, in practice, whatever the ministry in session says it requires, and every disputed artifact case renews that authority. Leaving the office or the faith would dissolve the officeholder's standing, kin ties, and inherited land relationships at once.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, ordnung_ministry, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, ordnung_ministry, beneficiary).

% Household heads and their families who live under the district's forbidden-artifact list: they heat and light with approved means, wear sewn-to-pattern dress, work with horse or approved machinery, and submit every prospective purchase to the question of whether it looks like something the English use. They receive mutual aid, barn raisings, old-age and burial security, and a settled identity in return. A member who adopts a forbidden artifact faces confession, and if unrepentant, shunning that extends to table and trade with kin. Leaving entirely means forfeiting family contact, livelihood networks, and the assurance of salvation they were raised inside.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, rank_and_file_members, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, rank_and_file_members, beneficiary).

% Adolescents between school and baptism who are permitted to sample English life — cars, phones, city work — before choosing baptism. During this window they can move between worlds; after baptism the same choices become defiance subject to shunning. Most return and accept the marker regime; a minority drift out permanently. Their consent to the artifact rules is therefore real but collected at the moment of maximum social dependence, before adult resources, savings, or perspective exist.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, rumspringa_youth, payer,
    powerless, immediate, mobile, local).

% Member families who chafe at specific prohibitions — typically the ones with no discernible function rationale, such as off-grid solar or synthetic work clothes. They bend rules covertly (a phone kept in the shop, panels on a back building), petition ministers informally, and in sharp episodes anchor a faction that threatens to split the district. Open agitation risks shunning, so their resistance runs through private noncompliance and quiet coalition-building rather than public argument; a successful split founds a new district with a revised list, at the cost of severed ties with kin who stayed.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, dissenting_households, payer,
    moderate, biographical, constrained, regional).

% Solar installers, textile mills, and equipment dealers who sell to rural off-grid customers generally. The artifact rule bars them from the community's business even where their product would leave the buyer fully disconnected from grid and supply chains: a panel that would cut a household's last utility bill is unsellable because of what it looks like mounted on a roof. They lose the market but do not otherwise depend on it, and they sell freely to the looser districts next door.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, offgrid_technology_suppliers, excluded,
    powerful, biographical, arbitrage, continental).

% Neighboring settlements of the same tradition whose Ordnung admits functionally isolated technology — off-grid solar, some modern fabrics — under a different reading of the same kernel. Their thriving demonstrates that the marker regime is one enforceable arrangement among several rather than an inevitability; members cite them in private, and strict ministries treat them as cautionary examples of drift. They are not party to this district's deliberation and have no seat in it.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, looser_ordnung_districts, excluded,
    institutional, generational, mobile, regional).

% Scholars of Anabaptist and communitarian groups who document Ordnung change, shunning practice, retention rates, and the artifact disputes across districts and decades. They see the full structure — the shared kernel, the competing readings, the enforcement economics — and hold no seat in any district's deliberation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, religious_studies_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, ordnung_ministry).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a mutually legible boundary between the community and English society: shared dress, a common forbidden-artifact list, and household technology norms give every member the same visible signals, so members recognize one another instantly, outsiders can identify the community, and assimilation drift becomes visible early enough to correct. It also coordinates expectations across households — no member's home announces a standard the others did not agree to.
% TRANSFER_FUNCTION: Moves conformity and the surrender of private judgment about dress and technology from individual members to the district ministry; moves the welfare costs of forgone artifacts (off-grid power, durable modern fabrics, communications) onto member households; returns mutual-aid eligibility, belonging, and salvific assurance, all conditioned on continued compliance.
% ABSENT_VOICES: Ex-members carry the sharpest objection — many describe the artifact rules as the specific burden that drove them out — and they are structurally erased: shunned, sometimes cut off by their own families, with no seat in any deliberation. Rumspringa youth consent before they have adult standing. Neighboring districts running looser Ordnungs are outside the conversation entirely, and the suppliers who would bid for the community's business are simply barred from it.
% DISAPPEARANCE_RATIONALE: If the forbidden-artifact list and its enforcement vanished overnight, household technology and dress would diversify within months along each family's preferences and budget; the ministry's adjudicative caseload — its principal day-to-day function — would largely evaporate; mutual aid and worship would continue, but the community's visible distinctness would fade over a generation as markers converged on English defaults, and the boundary would have to be rebuilt around dialect, occupation, and worship practice or be lost.
% FOUNDING_PROBLEM: Keeping the community separate from 'the world' — nonconformity to English society as a religious obligation — under mounting economic and technological entanglement: cars, grid electricity, mass-produced clothing, and telephones each threatened to dissolve the visible difference the tradition understood as obedience.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated from outside the benefiting parties: a century of sociological study of Anabaptist communities documents sustained assimilation pressure and records the Ordnung system arising as the response, and ex-member testimony independently attests both the pressure and the felt weight of the marker rules. No source outside the benefiting parties, however, corroborates the artifact reading's specific claim that resemblance to English artifacts is the necessary criterion — neighboring districts running function-based criteria refute that necessity from inside the same tradition, and that non-corroboration is itself signal.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the costs imposed on members are decoupled from function: forbidding an off-grid solar array reduces entanglement by exactly zero while imposing real welfare cost, so by the reading's own accounting the cost buys only appearance. Suppression is higher still (0.88) because persistence depends on shunning, kin severance, and mutual-aid eligibility — the constraint is held up by making exit socially lethal; note suppression is a raw structural property and is not scaled by power or scope anywhere in the computation. Theater is low (0.18): the practice is lived daily, not performed; the markers are functional within the frame, and the slow creep tracks enforcement-as-demonstration episodes rather than hollow ritual. Accessibility collapse is 0.7: inside the community the alternative space closes almost completely once the rule is understood, but exit remains nominally available at identity-destruction cost, so alternatives do not collapse to natural-law completeness. Resistance is 0.45: real but fragmented — hidden devices, youth attrition, episodic schisms — and structurally capped, because open resistance triggers the very sanction that punishes it. The three measurement series run on one shared time grid (points 0, 10, 20, 30, 40, 50) so every metric is authored at every examined time point; the smooth trajectories average over episodic tightening ratchets (post-incident Ordnung revisions after visible drift or scandal), which is why the series rise monotonically rather than stepping.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the ministry seat the regime is covenant faithfulness it stewards: the forbidden list is the community's promise made visible, and each new-artifact ruling is pastoral care. From the rank-and-file seat the same structure operates as a function-blind tax on household welfare enforced by the threat of losing everyone. The youth seat experiences a trap with a closing window: choices that are sanctioned sampling before baptism become damning defiance after it. Same-level divergence: two members of equal nominal standing differ sharply in effective position — dissenting households convert private disagreement into factional coalition power (schism threat is the only leverage the structure permits), while compliant households hold none; formally identical exit options (leaving means losing kin, livelihood, and raised-in assurance) carry wildly different subjective costs depending on household conviction intensity, which is why identity-lock, not resource poverty, is the binding exit barrier here. Coalition potential for the powerless is real but deliberately expensive: the structure routes all collective voice through ministry councils, so the coalition route is exit-shaped (splitting the church), which disciplines it.
 *
 * DIRECTIONALITY LOGIC:
 *   The ministry sits at the beneficiary end: it collects interpretive authority and agenda control from the very disputes the regime generates, and it writes the rules it enforces. Rank-and-file members are dual-listed (beneficiary of the boundary, victim of the function-blind prohibitions) but their NET structural position is target-side — the derivation chain would average the dual listing toward symmetry, which misreads them, so a directionality override sets the powerless atom to 0.68; every powerless seat in this story is a net payer, making the atom-level override safe. Rumspringa youth are targets with temporarily elevated mobility — their d is high now and rises steeply at baptism, when their exit option collapses from mobile to locked. Off-grid suppliers and looser districts are excluded rather than coordinated: their exclusion is the enforcement object itself, exactly as a barred rival rail is the enforcement object of a payment-exclusivity rule. The observer seat takes no directional position.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy here in the ordinary sense: the founding problem — keeping a separatist community distinct under relentless technological and economic entanglement — is live, corroborated externally, and pressing. The danger runs the other way: because the problem is live, extraction gets LAUNDERED as necessity — every function-blind prohibition is defended by pointing at the real assimilation threat, so the genuine coordination function (boundary legibility) continuously renews the license for the asymmetric cost-bearing riding on it. The tangled_rope claim is precisely what keeps both halves visible: the coordination function is real and would be mislabeled as pure extraction by a snare-only reading, while the function-blind prohibitions would be mislabeled as innocent coordination cost by a rope-only reading. The lifecycle watch: if the founding problem ever dies (complete assimilation or complete withdrawal), the marker regime would persist by inertia and discipline habit — the theater_ratio series and the founding-problem-status mismatch check are the instruments that would date that transition; the current flat-low theater trajectory says the transition has not begun.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the gelassenheit_separation kernel. When district deliberations rule on contested artifacts case by case, do they actually track the artifact criterion (resemblance to English artifacts), or the sibling criteria — structural entanglement (principle_reading) or practice preservation (consequence_reading)?',
    'Code a sample of recorded Ordnung rulings on new artifacts across districts and decades for which criterion the ministry''s stated reasoning invokes; compare applied criterion against official doctrine.',
    'If deliberations track entanglement or practice-effect, the operative constraint is a sibling reading wearing artifact language, and this story''s high epsilon is misattributed — the true artifact-reading constraint is narrower than its doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which criterion the enforced regime actually applies: the kernel''s readings disagree and doctrine may not match practice.').

omega_variable(
    marker_constitutiveness_ambiguity,
    'Is visible distinction constitutive of the community''s identity — such that relaxing the marker rules would dissolve the community — or instrumental, protectable by other means such as dialect, occupation, and worship practice?',
    'Compare retention, mutual-aid density, and self-reported identity across districts whose Ordnungs differ sharply on markers but share language and liturgy.',
    'If constitutive, the artifact rule''s costs are partly the price of the community''s existence and the coordination side of the ledger strengthens; if instrumental, the rule is substitutable and its persistence reflects enforcement inertia, shifting computed type toward pure extraction at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marker_constitutiveness_ambiguity, empirical, 'Whether the visible-marker regime is load-bearing for community survival or replaceable by other boundary mechanisms.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (shunning, kin severance, mutual-aid dependency) or internalized (members experience compliance as salvific necessity and disobedience as damnation risk)?',
    'Post-exit trajectories of leavers: whether felt obligation, guilt, and perceived spiritual risk persist after material barriers are gone; compare with adult converts who entered without childhood formation.',
    'If a large share is internalized, effective suppression exceeds the structural measure and travels with the member past exit — exit-option assessments for every member seat overstate available mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized components of the shunning-backed suppression.').

omega_variable(
    artifact_function_threshold_instability,
    'Where is the operative line between resembling a worldly artifact and acceptable adaptation? Districts admit diesel engines, propane appliances, and certain disposables while forbidding off-grid solar panels and synthetic fabrics — a pattern that tracks negotiated precedent more than appearance distance alone.',
    'Compile district admitted/forbidden lists and code each item for appearance-distance versus functional isolation; test which variable predicts admission.',
    'If function secretly licenses exceptions, the operative constraint is already hybrid and this reading''s epsilon is inflated relative to practice; if appearance genuinely decides, the function-blind cost-bearing is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_function_threshold_instability, empirical, 'Whether the appearance criterion is stable in application or already leaky toward function-based exceptions.').

omega_variable(
    receipt_seat_vs_diffuse_identity_good,
    'Does the ministry seat actually capture the regime''s gains (interpretive authority, agenda control over the boundary definition), or do the gains accrue diffusely to all members as collective identity and mutual insurance?',
    'Trace who bears opportunity cost and who gains positional goods when marker disputes arise: ministry tenure, deference flows, and agenda outcomes versus member-level welfare effects.',
    'If diffuse, the arrangement reads as uncaptured coordination-with-costs rather than ministry-captured extraction, and the receipt-surface classification shifts accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(receipt_seat_vs_diffuse_identity_good, conceptual, 'Whether the gains concentrate in the ministry seat or diffuse across the membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__artifact_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__artifact_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__artifact_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__artifact_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__artifact_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__artifact_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__artifact_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__artifact_reading, base_extractiveness, 50, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__artifact_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__artifact_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__artifact_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__artifact_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Amish technology restrictions' (or 'separation from the world') covers three structurally distinct claims with different epsilon, different victim sets, and different failure modes. This story (artifact_reading) is downstream in warrant: artifact-strict ministries borrow the principle reading's anti-entanglement language to defend appearance rules that reduce no entanglement, while the consequence reading functions as the tradition's internal reform pressure. The stories are linked pairwise through affects_constraints; each carries its own stable epsilon and must never be merged, since measuring the family through any single observable yields observer-dependent epsilon and violates DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
