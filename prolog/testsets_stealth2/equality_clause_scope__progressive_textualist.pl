% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Amendment-Gated Equality Scope (Progressive Textualist Reading)
 *   domain: constitutional law/political philosophy/civil rights history
 *
 * SUMMARY:
 *   This story authors the progressive_textualist reading of the
 *   constitutional equality kernel as a single epsilon-invariant constraint.
 *   The standing arrangement under contest: the constitutional text contains
 *   an equality principle whose application scope is bounded by the text as
 *   formally amended, with the Article V supermajority process as the sole
 *   legitimate channel of expansion; courts apply the equality guarantees
 *   where the amended text speaks but may not derive new scope from moral
 *   premises the text does not contain. The reading's own assessment of this
 *   arrangement is a moderate legitimacy threshold — bounded universalism
 *   that balances the original limits of the compact against a real capacity
 *   for revision. Extraction is real but bounded: the gate taxes
 *   change-seekers with a supermajority price and leaves dispersed minorities
 *   priced out, yet the same channel has delivered durable, consent-won
 *   inclusion (the Reconstruction Amendments, the Nineteenth). Sibling
 *   readings of the same kernel — restrictive_originalist and
 *   expansive_universalist — instantiate different constraints with different
 *   victim sets and different epsilon values, and are authored as separate
 *   files linked through the network section.
 *
 * KEY AGENTS:
 *   - - congressional_amendment_initiators: Agenda setter (institutional/constrained) — controls which expansions ever become votable
 *   - - state_ratifying_bodies: Agenda setter with beneficiary position (institutional/constrained) — holds the three-fourths veto, retains scope control
 *   - - textualist_courts: Enforcement arm (institutional/constrained) — applies the amended text, declines scope creation
 *   - - existing_political_majorities: Primary beneficiary (powerful/mobile) — collects pacing stability at near-zero cost
 *   - - durable_rights_recipients: Entrenched beneficiary (moderate/identity_locked) — holds amendment-written standing
 *   - - civil_rights_movements: Organized payer (organized/trapped) — bears the decade-scale cost of supermajority assembly
 *   - - coalition_disadvantaged_minorities: Primary payer (powerless/trapped) — priced out of inclusion by the arithmetic itself
 *   - - judicial_expansion_advocates: Excluded payer (powerful/constrained) — channel permanently disqualified
 *   - - comparative_constitutional_scholars: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.55).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.42).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.55).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Amendment-Gated Equality Scope (Progressive Textualist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional law/political philosophy/civil rights history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '19eec9a4-c599-49e6-b087-42b1f3143153').
narrative_ontology:cs_kernel_codification('19eec9a4-c599-49e6-b087-42b1f3143153', fixed_text).
narrative_ontology:cs_authority_grounding('19eec9a4-c599-49e6-b087-42b1f3143153', lineage).
narrative_ontology:cs_interpretation_layer_present('19eec9a4-c599-49e6-b087-42b1f3143153').
narrative_ontology:cs_reading_relation('19eec9a4-c599-49e6-b087-42b1f3143153', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('19eec9a4-c599-49e6-b087-42b1f3143153', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('19eec9a4-c599-49e6-b087-42b1f3143153', foundational, amended_text_bounds_equality_scope).
narrative_ontology:cs_axiom_status(amended_text_bounds_equality_scope, holdable).
narrative_ontology:cs_axiom_grounding('19eec9a4-c599-49e6-b087-42b1f3143153', amended_text_bounds_equality_scope, conventional).
narrative_ontology:cs_axiom('19eec9a4-c599-49e6-b087-42b1f3143153', foundational, supermajority_consent_legitimates_expansion).
narrative_ontology:cs_axiom_status(supermajority_consent_legitimates_expansion, holdable).
narrative_ontology:cs_axiom_grounding('19eec9a4-c599-49e6-b087-42b1f3143153', supermajority_consent_legitimates_expansion, conventional).
narrative_ontology:cs_axiom('19eec9a4-c599-49e6-b087-42b1f3143153', secondary, amendment_route_yields_durable_rights).
narrative_ontology:cs_axiom_status(amendment_route_yields_durable_rights, holdable).
narrative_ontology:cs_axiom_grounding('19eec9a4-c599-49e6-b087-42b1f3143153', amendment_route_yields_durable_rights, instrumental).
narrative_ontology:cs_reference_frame('19eec9a4-c599-49e6-b087-42b1f3143153', amended_text_canonical_scope).
narrative_ontology:cs_drift_state('19eec9a4-c599-49e6-b087-42b1f3143153', contemporary_judicial_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('19eec9a4-c599-49e6-b087-42b1f3143153', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, existing_political_majorities).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, durable_rights_recipients).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, state_ratifying_bodies).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, civil_rights_movements).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, coalition_disadvantaged_minorities).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_expansion_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of Congress who originate proposed amendments touching the equality provisions. Expansion reaches the states only if two-thirds of each chamber agrees, so this seat decides which expansions ever become nationally votable. Proposing unpopular expansions spends political capital; the reward is agenda control over the pace of constitutional change.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, congressional_amendment_initiators, agenda_setter,
    institutional, generational, constrained, national).

% State legislatures and ratifying conventions that accept or reject proposed amendments; no equality expansion takes effect without three-fourths of them. Each state holds an absolute veto over national scope, which preserves state control over the terms of membership in the compact. They administer the gate and simultaneously collect the benefit of retained control.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, state_ratifying_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, state_ratifying_bodies, beneficiary).

% Federal and state courts applying the equality guarantees where the amended text speaks. Their mandate under this arrangement is application, not derivation: they enforce equal-protection and suffrage guarantees as written and as amended, and decline to extend scope from moral premises the text does not contain. Their enforcement workload rises whenever litigants press for scope creation from the bench.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, textualist_courts, agenda_setter,
    institutional, generational, constrained, national).

% Current voting coalitions that benefit from predictable, consent-paced change. They bear almost none of the gate's costs: the status quo persists until supermajorities form, and they can shift allegiances between electoral cycles while the ground rules stay fixed.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, existing_political_majorities, beneficiary,
    powerful, biographical, mobile, national).

% Groups whose inclusion was carried into the text by ratified amendments — freedmen and their descendants through the Reconstruction Amendments, women through the Nineteenth. Their standing is written into the instrument itself, which makes it unusually secure. Their civic identity is constituted by that amendment-won settlement; repudiating the amendment channel would mean repudiating the source of their own recognition.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, durable_rights_recipients, beneficiary,
    moderate, generational, identity_locked, national).

% Organized campaigns for expansion that must operate exclusively through the amendment channel. Their work is measured in decades: assembling coalitions large enough to move two-thirds of Congress and three-fourths of the states, sustaining them across election cycles, and absorbing repeated defeat. They cannot exit the constitutional order they are trying to revise.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, civil_rights_movements, payer,
    organized, generational, trapped, national).

% Groups whose size or geographic dispersion leaves them short of the arithmetic the gate demands — too few voters, spread across too many states, to ever clear three-fourths of the legislatures. For them the price of inclusion exceeds any achievable campaign, and there is nowhere outside the compact to take their claim.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, coalition_disadvantaged_minorities, payer,
    powerless, generational, trapped, national).

% Living-constitutionalist scholars, advocates, and impact litigators whose preferred channel — deriving expanded scope from the bench — is ruled out by the arrangement itself. They argue forcefully in public discourse and professional literature, but within this framework their arguments have no legitimate uptake, and their strategy bears the cost of permanent disqualification.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicial_expansion_advocates, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, judicial_expansion_advocates, excluded).

% Academic observers comparing amendment-gated and interpretation-driven revision across national constitutions. They neither collect nor pay under the arrangement; they document how different legitimacy thresholds shape the speed and durability of rights expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, existing_political_majorities).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, democratically controlled channel for redefining the scope of constitutional equality: fundamental law changes only when two-thirds of each congressional chamber and three-fourths of the states consent, solving the problem of who may legitimately redefine the terms of the compact.
% TRANSFER_FUNCTION: Moves scope-definition power from courts and litigants to supermajority legislative bodies; moves the cost of inclusion onto change-seeking groups, who must fund and sustain decade-scale coalition campaigns; moves stability and pacing control to existing majorities for as long as no supermajority forms.
% ABSENT_VOICES: Those whose inclusion the gate prices beyond reach — geographically dispersed minorities who can never clear three-fourths of state legislatures — are absent from ratification rooms by the same arithmetic the gate enforces. Historically, the enslaved and the disenfranchised had no vote on the amendments that later included them; their interests were represented only indirectly, by electorates that had excluded them.
% DISAPPEARANCE_RATIONALE: If the amendment gate vanished overnight, every settled expansion's legitimacy basis would collapse into litigation over who may now define scope, and the system would immediately rearrange around whichever actor seized the vacated channel — courts by default, since they are always in session. The movement infrastructure built for ratification politics would reorganize around litigation, and state-level veto players would lose their constitutional role entirely.
% FOUNDING_PROBLEM: How to make fundamental law correctable without making it unstable: Article V was designed so the compact's known flaws — slavery foremost — could be cured by supermajority consent rather than by revolution, secession, or judicial improvisation.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians working from Madison's notes and the Federalist (Nos. 49, 50, and 85) attest the design intent from outside any benefiting party; comparative constitutional scholars confirm that the problem of choosing a legitimate revision channel remains live in every democracy with entrenched fundamental law. The attestation does not rest on the beneficiary seats alone.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.55 because the gate's price is real and unevenly distributed: change-seekers pay in decades of organization, and the dispersed pay without any prospect of collection, yet the channel also delivers durable inclusion that cheaper channels plausibly would not. Suppression is 0.42 and structural rather than internalized — it consists in the raw arithmetic of Article V plus the courts' refusal to open a second channel, not in any cognitive fusion of the targets; the powerless seat's suppression is purely external. Theater is low (0.18): proposals, ratifications, and occasional deliveries are real work, not ritual, though ceremonial commemoration of past amendments has grown as a substitute for new ones. Accessibility collapse is moderate (0.50): within the framework the amendment route remains genuinely open, but the judicial alternative is closed by the reading's own commitment, so roughly half the option space survives. Resistance is 0.55: the gate is contested from both flanks — universalists attack the gate, originalists attack the expansions already admitted through it. The measurement series run on one shared time grid (1791, 1868, 1920, 1964, 1992, 2026) with every tracked metric authored at every point. Base extractiveness steps down sharply at 1868, when the Fourteenth Amendment delivered mass inclusion through the sanctioned channel, then climbs steadily afterward as the amendment route calcifies (the ERA failure is the emblematic case) while the set of groups seeking inclusion grows faster than supermajorities form. Suppression_requirement is tracked deliberately rather than left static because enforcement capacity is a live dynamic in this story: the machinery for holding the gate — textualist canons, the disciplined refusal of scope creation — hardened as judicial expansion became the dominant rival channel after the 1960s, which is exactly the enforcement-intensification trajectory the series records.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setting seats (congressional initiators, state ratifiers, textualist courts) the arrangement is the constitution working as designed: a demanding but legitimate price on changing fundamental law. From the trapped payer seats the same structure is a door priced shut — civil_rights_movements experience it as a lifetime of coalition labor with uncertain payoff, and coalition_disadvantaged_minorities experience it as exclusion with no campaign that could ever end it. durable_rights_recipients occupy a distinctive third position: their benefit is identity-constitutive, since their standing as recognized persons is written into the text by the very channel the arrangement protects; if that identity frame broke — if they came to see their inclusion as contingent on ongoing judicial protection rather than on the text — their seat would migrate toward the universalist constituency and the arrangement would lose its most loyal beneficiaries. judicial_expansion_advocates are powerful in general terms yet bear real costs here, because the arrangement disqualifies their specific vocation regardless of their general influence.
 *
 * DIRECTIONALITY LOGIC:
 *   existing_political_majorities and state_ratifying_bodies sit near the beneficiary end: the gate subsidizes them with stability and retained control while costing them almost nothing. civil_rights_movements and coalition_disadvantaged_minorities sit near the target end, and their trapped exits push them further toward it — there is no jurisdiction-shopping out of a constitutional order. durable_rights_recipients derive low directionality as declared beneficiaries, with the identity_lock reflecting that their benefit is constitutive rather than incidental. judicial_expansion_advocates are declared victims despite high general power: their constraint-specific relationship (barred channel, no legitimate uptake) fixes their position, not their resources. On the coalition question for the powerless seat: the escape route for coalition_disadvantaged_minorities would be exactly the cross-state coalition the gate prices, and the state-unit arithmetic of Article V is what defeats it — dispersion across many small-population states makes three-fourths unreachable no matter how motivated the group, so coalition power does not rescue this seat's directionality. Receipt: the gate's output — preserved status quo and pacing control — demonstrably accrues to existing_political_majorities, who are therefore named as the receipt seat; fixing the arrangement would require assembling the very supermajority the arrangement demands, so the cost class of fixing is prohibitive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a correctable-but-stable fundamental law — remains live, so nothing here is mandatrophy-resolved and no sunset applies. The classification discipline cuts in both directions. Reading the gate as pure coordination would erase the priced-out minority seat and launder a real exclusionary cost as mere transaction overhead. Reading it as pure extraction would erase what the channel has actually delivered: mass enfranchisements carried into the text by consent, which endure precisely because they were won through the gate. Tangled rope holds both facts — the same supermajority structure that legitimates expansion also prices it beyond reach for the dispersed — and the temporal series guards against lifecycle misreading in both directions: the 1868 step-down shows the coordination function firing, while the post-1964 climb in extractiveness and enforcement demand shows the extraction component accumulating as the gate calcifies. If a future series showed theater rising while deliveries ceased, the piton signature would become the live question; nothing in the current record supports it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the equality_clause_scope kernel; how would the restrictive_originalist or expansive_universalist readings change the structural classification?',
    'Author the sibling stories and compare computed types across the family: restrictive_originalist shifts the victim set to everyone outside the eighteenth-century franchise and raises epsilon sharply; expansive_universalist removes the amendment gate entirely, lowering suppression while raising resistance from democratic-process defenders.',
    'If the universalist reading computes as rope while this reading computes as tangled_rope, the extraction component is attributable specifically to the supermajority gate rather than to the equality principle itself; if the originalist reading computes as snare, the bounded-universalism compromise is vindicated as the least-extractive live reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame uncertainty: classification is indexed to this reading of a contested kernel.').

omega_variable(
    supermajority_threshold_justice,
    'Is the three-fourths-of-states threshold a fair price of democratic legitimacy or a structural veto that entrenches regional majorities against geographically dispersed minorities?',
    'Comparative analysis of amendment outcomes against counterfactual thresholds (two-thirds national vote, simple-majority-plus-persistence): if dispersed-minority inclusion succeeds only under lower thresholds across comparable federations, the gate operates as entrenchment rather than deliberation.',
    'An entrenchment finding raises effective extraction toward the full-target end for powerless seats and pushes operation toward snare-flavored dynamics; a fair-price finding supports the coordination reading and keeps the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_threshold_justice, empirical, 'Whether the ratification arithmetic is legitimate pricing or minority exclusion.').

omega_variable(
    application_reinterpretation_boundary,
    'Where does applying the amended text end and reinterpreting it begin — was incorporating the Bill of Rights against the states, or desegregating schools under the Fourteenth, application of existing scope or creation of new scope?',
    'Doctrinal analysis separating textual warrant from extra-textual moral premise in the landmark expansion rulings: rulings traceable to text added by ratified amendments count as application; rulings resting on premises the text does not contain count as creation.',
    'A wide-application reading shrinks the suppression profile, since most twentieth-century expansion then falls inside the allowed channel; a narrow reading raises suppression substantially, because the dominant twentieth-century expansion route was the disallowed one and enforcement against it intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(application_reinterpretation_boundary, conceptual, 'Boundary ambiguity between sanctioned application and forbidden judicial scope creation.').

omega_variable(
    era_failure_diagnostic,
    'Does the Equal Rights Amendment''s failure — congressional passage followed by deadline lapse without ratification — show the gate working as designed (consent legitimately withheld) or malfunctioning (an arbitrary deadline defeating sustained majority will)?',
    'Polling and ratification-record analysis across the 1972-1982 window: if sustained national majorities favored ratification while state-level arithmetic blocked it, the gate diverged from national consent rather than aggregating it.',
    'A malfunction finding raises extraction for change-seeking seats without adding coordination value, degrading the legitimacy-threshold defense; an as-designed finding supports the bounded-universalism framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(era_failure_diagnostic, empirical, 'Whether the ERA episode evidences gate fidelity or gate failure.').

omega_variable(
    amendment_durability_premise,
    'Is amendment-won inclusion actually more durable than court-won recognition, as the reading''s instrumental defense requires — or can judicially recognized rights outlast some amendments (prohibition''s repeal) while court-won rights persist for generations?',
    'Longitudinal comparison of reversal rates for amendment-origin versus court-origin rights across jurisdictions and eras, controlling for the underlying strength of supporting coalitions.',
    'If durability parity holds, the reading loses its strongest instrumental justification for the gate''s cost, and the extraction component stands with less defense; if amendment-won rights are markedly more durable, the supermajority price buys real value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_durability_premise, empirical, 'Empirical test of the durability advantage claimed for the amendment channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqscope_prog_textualist_tr_t1791, equality_clause_scope__progressive_textualist, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(eqscope_prog_textualist_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.12).
narrative_ontology:measurement(eqscope_prog_textualist_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.14).
narrative_ontology:measurement(eqscope_prog_textualist_tr_t1964, equality_clause_scope__progressive_textualist, theater_ratio, 1964, 0.16).
narrative_ontology:measurement(eqscope_prog_textualist_tr_t1992, equality_clause_scope__progressive_textualist, theater_ratio, 1992, 0.17).
narrative_ontology:measurement(eqscope_prog_textualist_tr_t2026, equality_clause_scope__progressive_textualist, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(eqscope_prog_textualist_be_t1791, equality_clause_scope__progressive_textualist, base_extractiveness, 1791, 0.62).
narrative_ontology:measurement(eqscope_prog_textualist_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.44).
narrative_ontology:measurement(eqscope_prog_textualist_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.47).
narrative_ontology:measurement(eqscope_prog_textualist_be_t1964, equality_clause_scope__progressive_textualist, base_extractiveness, 1964, 0.51).
narrative_ontology:measurement(eqscope_prog_textualist_be_t1992, equality_clause_scope__progressive_textualist, base_extractiveness, 1992, 0.53).
narrative_ontology:measurement(eqscope_prog_textualist_be_t2026, equality_clause_scope__progressive_textualist, base_extractiveness, 2026, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(eqscope_prog_textualist_su_t1791, equality_clause_scope__progressive_textualist, suppression_requirement, 1791, 0.35).
narrative_ontology:measurement(eqscope_prog_textualist_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(eqscope_prog_textualist_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.32).
narrative_ontology:measurement(eqscope_prog_textualist_su_t1964, equality_clause_scope__progressive_textualist, suppression_requirement, 1964, 0.38).
narrative_ontology:measurement(eqscope_prog_textualist_su_t1992, equality_clause_scope__progressive_textualist, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(eqscope_prog_textualist_su_t2026, equality_clause_scope__progressive_textualist, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'constitutional equality.' The single natural-language concept covers three structurally distinct claims that differ on initial scope and expansion channel, so it decomposes into three stories sharing the equality_clause_scope kernel: restrictive_originalist (upstream-historical; highest epsilon, victim set spans everyone outside the founding franchise), progressive_textualist (this file; bounded universalism with a supermajority gate, moderate epsilon), and expansive_universalist (downstream-contested; no gate, lowest epsilon from its own seat but highest resistance from process defenders). The upstream reading is typically cited as evidence in disputes over the downstream ones; each story links the others through affects_constraints, and each carries its own epsilon, beneficiaries, and victims per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
