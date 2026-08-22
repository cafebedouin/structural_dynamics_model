% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading — Categorical Shield for Personal Firearm Ownership
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The ratified 1791 Second Amendment text is a contested kernel; this story
 *   instantiates one reading of it — the individual-right reading, under
 *   which the operative clause secures a pre-existing individual right to
 *   keep and bear arms unconnected to militia service, and the prefatory
 *   militia clause states purpose without limiting scope. The standing
 *   arrangement this story measures is the post-Heller regime as extended by
 *   McDonald (2010), Bruen (2022), and Rahimi (2024): categorical
 *   constitutional protection for personal firearm ownership, judicial
 *   enforcement under a text-history-tradition test, and the corresponding
 *   removal of whole classes of regulatory instruments from every government
 *   in the jurisdiction. The claim/metric gap is deliberate and
 *   reading-indexed: this reading CLAIMS the arrangement as a rights-shield
 *   (its own framing is a pure coordination good), while the authored metrics
 *   record the structural ledger — categorical foreclosure of regulatory
 *   alternatives, externalized safety costs, concentrated commercial gains —
 *   that the engine weighs per seat. Sibling readings
 *   (collective_right_reading, civic_right_reading) are separate constraint
 *   files linked through network.affects_constraints; their epsilon values
 *   over the same referent are expected to differ by design (OQ-26). KEY
 *   AGENTS (by structural relationship): - law_abiding_individual_owners:
 *   primary beneficiary (organized/identity_locked) — holds the protected
 *   liberty; exit costs inflated by identity fusion - firearms_industry:
 *   concentrated commercial beneficiary (powerful/arbitrage) — captures the
 *   monetizable gains; the seat named by gain_flow -
 *   state_regulatory_authorities: primary payer (institutional/trapped) —
 *   loses categorical regulatory instruments; cannot exit constitutional
 *   supremacy - gun_violence_burdened_communities: payer (powerless/trapped)
 *   — absorbs externalized safety costs; coalition leverage blocked by
 *   entrenchment - intimate_partner_violence_survivors: payer
 *   (powerless/trapped) — protection contingent on litigation they do not
 *   control - federal_judiciary: agenda_setter (institutional/analytical) —
 *   administers and defines the settlement; custodian of scope -
 *   gun_rights_advocacy_orgs: beneficiary with agenda-shaping secondary role
 *   (organized/identity_locked) — repeat-player case selection -
 *   gun_control_advocacy_coalition: payer (organized/identity_locked) —
 *   foreclosed policy program; permanent opposition seat - legal_academy:
 *   analytical observer (analytical/analytical) — supplies the historical
 *   record courts consume - prospective_firearm_owners: peripheral
 *   beneficiary (moderate/mobile) — guaranteed open door, cheap exit
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.35).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.7).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.13).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading — Categorical Shield for Personal Firearm Ownership").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '36739ebb-e775-45e2-9fd0-608c98779fce').
narrative_ontology:cs_kernel_codification('36739ebb-e775-45e2-9fd0-608c98779fce', fixed_text).
narrative_ontology:cs_authority_grounding('36739ebb-e775-45e2-9fd0-608c98779fce', lineage).
narrative_ontology:cs_interpretation_layer_present('36739ebb-e775-45e2-9fd0-608c98779fce').
narrative_ontology:cs_reading_relation('36739ebb-e775-45e2-9fd0-608c98779fce', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('36739ebb-e775-45e2-9fd0-608c98779fce', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('36739ebb-e775-45e2-9fd0-608c98779fce', foundational, individual_right_independent_of_militia_service).
narrative_ontology:cs_axiom_status(individual_right_independent_of_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('36739ebb-e775-45e2-9fd0-608c98779fce', individual_right_independent_of_militia_service, deontological).
narrative_ontology:cs_axiom('36739ebb-e775-45e2-9fd0-608c98779fce', foundational, prefatory_clause_does_not_limit_operative_text).
narrative_ontology:cs_axiom_status(prefatory_clause_does_not_limit_operative_text, holdable).
narrative_ontology:cs_axiom_grounding('36739ebb-e775-45e2-9fd0-608c98779fce', prefatory_clause_does_not_limit_operative_text, conventional).
narrative_ontology:cs_axiom('36739ebb-e775-45e2-9fd0-608c98779fce', secondary, text_history_tradition_review_replaces_balancing).
narrative_ontology:cs_axiom_status(text_history_tradition_review_replaces_balancing, holdable).
narrative_ontology:cs_axiom_grounding('36739ebb-e775-45e2-9fd0-608c98779fce', text_history_tradition_review_replaces_balancing, conventional).
narrative_ontology:cs_reference_frame('36739ebb-e775-45e2-9fd0-608c98779fce', preexisting_natural_right_fixed_by_original_meaning).
narrative_ontology:cs_drift_state('36739ebb-e775-45e2-9fd0-608c98779fce', post_bruen_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('36739ebb-e775-45e2-9fd0-608c98779fce', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, law_abiding_individual_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, prospective_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_regulatory_authorities).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_burdened_communities).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, intimate_partner_violence_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_orgs).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_control_advocacy_coalition).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, heller_individual_right_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, text_history_tradition_review).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, preexisting_natural_right_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tens of millions of adults who keep handguns, rifles, and shotguns for self-defense, hunting, sport, and collection. The constitutional guarantee ensures no legislature or city council can prohibit their possession outright; what they owe in return is ordinary background-check compliance and liability for misuse. Leaving means selling the hardware and accepting that re-acquisition depends on political weather they do not control; for the core constituency, ownership is bound up with family tradition, regional identity, and political self-description, which makes exit costly far beyond resale value.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, law_abiding_individual_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Adults who do not currently own firearms but may choose to. The arrangement guarantees them an open door: whatever restrictions exist, outright prohibition is off the table. Their stake is cheap to enter and cheap to abandon — they can simply not buy — but the guarantee permanently shapes their option set.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, prospective_firearm_owners, beneficiary,
    moderate, immediate, mobile, national).

% Manufacturers, wholesalers, dealers, and accessory makers. The civilian market the guarantee protects is their revenue base; prohibition risk disappears from production planning under the arrangement. Monetizable gains concentrate here: unit sales, accessory ecosystems, and a litigation environment where trade associations can sue to unwind restrictions. Portfolio exit is easy — product lines can pivot — but the industry's trade groups are organizationally fused with defending the arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, national).

% Legislatures, city councils, police departments, and administrative agencies that write and enforce firearms regulation. Whole categories of instruments are removed from their toolkit: bans on weapon classes, discretionary carry regimes, and broad denial schemes are presumptively unavailable, and what remains must be defended by historical argument rather than evidence of efficacy. They cannot exit constitutional supremacy; their recourse is drafting within narrower bounds, litigating test cases, or waiting for judicial composition to change.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_regulatory_authorities, payer,
    institutional, generational, trapped, regional).

% Neighborhoods, disproportionately urban and low-income, where shootings concentrate. They absorb the mortality, injury, trauma, and policing costs of widespread armedness, and they hold no lever that reaches the arrangement: local ordinance is preempted, state law is bounded, and their remedy runs through constitutional amendment or judicial turnover — channels where their voting weight is diluted. Residential exit is theoretically open and practically expensive.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_burdened_communities, payer,
    powerless, generational, trapped, local).

% People whose safety depends on keeping firearms away from current or former partners. Statutes doing this work are among the most litigated provisions in the post-Bruen environment; their protection now hangs on whether judges find sufficiently close historical analogues — an argument they do not control and often cannot afford to join. Their exit from danger typically requires the legal system's help, which the arrangement narrows.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, intimate_partner_violence_survivors, payer,
    powerless, immediate, trapped, local).

% Federal courts, culminating in the Supreme Court, define what the arrangement protects and what it permits, case by case. Every regulatory experiment in fifty states passes through their dockets; emergency applications and circuit splits give them continuous agenda control. Lifetime tenure insulates them from retaliation by any seat unhappy with the outcome. They neither gain nor pay in the ordinary sense; their stake is custody of the settlement's meaning.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Membership organizations and litigation foundations that exist to defend and expand the arrangement. Dues, litigation victories, and fundraising cycles all flow through its continued salience; their institutional identity is fused with it. As repeat players they select the test cases that reach the courts, giving them agenda-shaping power disproportionate to their headcount.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_advocacy_orgs, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, gun_rights_advocacy_orgs, agenda_setter).

% Advocacy organizations, philanthropies, and electoral movements seeking stronger regulation. The arrangement forecloses their preferred policy endpoints regardless of electoral success, diverting their resources into litigation designed to carve exceptions rather than enact programs. Their identity is likewise fused with opposition, which keeps them in the game through losses that would dissolve a purely instrumental coalition.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_control_advocacy_coalition, payer,
    organized, biographical, identity_locked, national).

% Historians and constitutional scholars producing the eighteenth-century record that courts now consume as regulatory evidence. Their analyses supply the historical traditions that decide real cases; they hold no direct stake in outcomes, but their output is load-bearing for every seat, and rival schools of historiography map onto the political dispute.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Entrenches a settlement of a recurring political conflict: by placing firearm possession beyond ordinary legislative reach and entrusting its boundaries to courts applying fixed text, it assures individuals that shifting regulatory majorities cannot disarm them, and gives buyers and sellers a stable legal environment that statute-by-statute bargaining could not provide.
% TRANSFER_FUNCTION: Moves regulatory discretion over firearms from legislatures, municipalities, and administrative agencies to individual holders and, incidentally, to commercial manufacturers and dealers whose civilian market the guarantee protects; correspondingly leaves the costs of widespread armedness — violence response, medical burden, policing — with public budgets and the communities where shootings concentrate.
% ABSENT_VOICES: Survivors of gun violence and bereaved families who never appear as parties in the cases that define the right's scope; residents of cities whose ordinances are struck by courts they did not elect; and, historically, the enslaved and colonially disarmed populations whose exclusion shaped whose arms-holding counted when the text was framed. They would object that the settlement prices their safety as the cost of others' liberty; they are absent because constitutional litigation filters voices through standing, resources, and case selection controlled by the organized advocacy seats.
% DISAPPEARANCE_RATIONALE: Overnight repeal or abandonment would trigger an immediate legislative flood in both directions — some states enacting prohibition and licensing regimes, others codifying protections — a repricing of the firearms market, restructuring of the advocacy landscape, and a decade of litigation over reliance interests covering the tens of millions of existing firearms. Nothing about the current allocation of regulatory authority survives contact with its removal.
% FOUNDING_PROBLEM: The founding generation's settlement of two fears fused in one text: that a distant federal government might disarm the citizen militias on which republican defense depended, and that government generally might strip individuals of arms already understood as theirs. The amendment was drafted to assure both states and individuals that disarmament was off the federal menu.
% FOUNDING_PROBLEM_CORROBORATION: Academic constitutional historians working outside the beneficiary set (the militia-centric scholarship literature) attest the militia-structure purpose and judge that fear obsolete in the National Guard era; the reading's own judicial proponents (the Heller-majority lineage) attest a perennial anti-disarmament and self-defense purpose as still live. Corroboration exists on both sides and sits outside the benefiting parties on the obsolescence side; agreement on a single status does not exist, which is why the status is authored contested rather than dead.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).
:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.35 — reading-indexed per OQ-26: the referent is the standing post-Bruen arrangement, and the value is what THIS reading's own lights yield. The reading treats restored individual liberty as restitution rather than transfer, which discounts the largest flow (regulatory capacity moved from governments to holders) to near zero; what survives is the residue the reading itself concedes — incidental commercial shielding beyond defense needs, enforcement overriding regulatory judgments even the reading accepts as legitimate, and the gap between categorical breadth and the reading's own conceded limits (Heller's presumptively lawful prohibitions, Rahimi's dangerous-person carve-out). The manifest seeded a mod-high bin; the divergence is documented in omega epsilon_gradient_across_readings and is expected to resolve as a cross-reading gradient once the sibling files author their own values over the shared referent. Suppression (0.70) is a raw structural property, unscaled by power or scope: constitutional supremacy plus an activated judiciary categorically forecloses regulatory alternatives for every government while imposing nothing on holders. Theater (0.13 at interval end) traces a U-shaped arc — the arrangement was celebrated and unenforced for most of the twentieth century (theater peaking at 0.60 around the Miller-era dormancy) and became abruptly functional after Heller; the series documents that phase transition, not noise. Accessibility collapse (0.68) reflects how completely the text-history-tradition test closes regulatory alternatives short of constitutional amendment; resistance (0.72) records the continuous organized pushback — test-case litigation, workaround statutes, circuit splits — the arrangement provokes. All three tracked metrics are authored on one shared ten-point grid (1791-2025) so no row borrows an end-state value from another metric's timeline.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same text as different arrangements. From the federal bench it is a custody problem: scope-setting case by case, insulated by tenure. From state capitols it is a categorical amputation of the regulatory toolkit, experienced as a mandate to argue history rather than efficacy. From the owner constituency it is a shield whose value scales with identity investment. From violence-burdened neighborhoods it is a ceiling on self-government they cannot vote under. The two institutional seats share a power atom yet derive opposed directionalities — the differentiation runs through role and exit (custody with analytical exit versus administration under trapped supremacy), not through power. Likewise the two advocacy seats mirror each other: equal organizational power, opposite identity locks, and the arrangement is the fixed point both orbit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place holders, prospective acquirers, and the industry at the subsidized end; the industry's arbitrage-grade exit pushes it nearest the full-beneficiary pole, which is why the receipt surface names it — the monetizable gains demonstrably land there even though the liberty gains are diffuse across holders. Payer declarations place regulators, violence-burdened communities, and IPV survivors at the target end; trapped exit for all three means the engine should compute them near the full-target pole, with the powerless seats (communities, survivors) at the extreme because neither market nor ballot offers relief. Identity lock cuts both ways: it deepens subsidy for locked beneficiaries (owners, rights organizations) and deepens exposure for the locked payer (control coalition). The judiciary's agenda_setter role carries no beneficiary or victim declaration; its directionality derives from administration rather than collection, and no directionality override is authored because the structural data already separates it from the institutional payers it rules against.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assurance against federal disarmament of the citizen militia — is obsolete in its original terms: the National Guard era dissolved the arrangement the prefatory clause presupposed. The constraint escaped mandatrophy death not by solving its founding problem but by doctrinal renovation: after a half-century of dormancy in which theater exceeded function (the 1939-1977 plateau in the series), Heller re-founded the arrangement on a self-defense genealogy the framers would recognize but did not center. Mandatrophy is therefore NOT resolved: the arrangement persists on a transplanted mandate while the founding problem's status is genuinely contested — the reading's proponents attest a perennial anti-disarmament purpose; historians outside the beneficiary set attest obsolescence. The mismatch consumer should read status=contested here as the honest state of a live genealogy war: if the civic or collective genealogy wins culturally, status flips to dead while the arrangement persists, arming the capture/zombie flag against a low-theater backdrop — a zombie with functioning machinery, which is the diagnostically interesting case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the individual_right_reading of the second_amendment_scope kernel; what structural features of the constraint would change under the sibling readings?',
    'Author the sibling files (collective_right_reading, civic_right_reading) over the same standing arrangement and compare beneficiary sets, victim sets, and epsilon values.',
    'Under the collective reading the beneficiary set collapses to state governments and epsilon rises sharply; under the civic reading the beneficiary set is conditioned on militia participation and coverage narrows. Classification of every seat shifts accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one kernel, three readings, three distinct constraints.').

omega_variable(
    prefatory_clause_disagreement_location,
    'Where exactly do the readings disagree — what structural element carries the contest?',
    'Original-public-meaning historiography of the prefatory clause''s limiting force, plus doctrinal adoption patterns (Heller''s express rejection of both rival readings).',
    'If the prefatory clause is read as limiting, this reading collapses into the civic variant (right conditioned on militia function) or the collective variant (no individual right); the entire beneficiary/victim structure and epsilon follow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_disagreement_location, conceptual, 'The disagreement is located in the limiting force of ''A well regulated Militia...'' on the operative text.').

omega_variable(
    epsilon_gradient_across_readings,
    'The manifest seeded a mod-high epsilon bin for this reading, while reading-lights analysis (OQ-26: values are reading-indexed over a fixed referent) yields low-moderate; which is correct for this seat?',
    'Cross-reading comparison once the sibling files author their own epsilon over the shared referent: the gradient (collective highest, civic intermediate, individual lowest) tests whether the referent was held fixed.',
    'If all three readings converge on one epsilon, the referent was confused with the topic and the family must be re-authored; if the gradient appears, the manifest bin is revealed as a structural (non-evaluative) estimate superseded by the reading-indexed value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_gradient_across_readings, conceptual, 'Documents the authored divergence from the manifest seed and the predicted cross-reading gradient.').

omega_variable(
    violence_cost_attribution,
    'Do the safety costs borne by exposed communities belong in the victim ledger as products of this arrangement, or are they attributable to criminal misuse that would persist under any regulatory regime?',
    'Econometric natural experiments across permitless-carry adoptions and struck ordinances, comparing violence trajectories against matched controls.',
    'Attribution to the arrangement raises epsilon, strengthens the victim set, and pushes computed per-seat types toward extraction; attribution to misuse shrinks both and vindicates the reading''s own accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_cost_attribution, empirical, 'Causal attribution dispute over the largest claimed cost flow.').

omega_variable(
    owner_identity_fusion_extent,
    'How much of the core owner constituency''s attachment to the arrangement is identity-fused rather than instrumental?',
    'Longitudinal survey panel tracking ownership motivation, family transmission, and stated willingness to surrender firearms under compensation.',
    'Higher fusion sustains resistance and entrenchment indefinitely and explains why electoral defeat does not erode the beneficiary coalition; instrumental ownership would make the constituency negotiable and lower long-run persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(owner_identity_fusion_extent, empirical, 'Identity-lock depth on the primary beneficiary seat.').

omega_variable(
    historical_analogue_elasticity,
    'How determinate is the text-history-tradition test''s analogue-matching requirement across circuits?',
    'Code five years of post-Bruen circuit outcomes on matched fact patterns; measure inter-circuit variance on identical regulatory designs.',
    'High elasticity destabilizes accessibility_collapse below its authored value and feeds practice drift; low elasticity hardens the arrangement and accelerates the enforcement ratchet.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_analogue_elasticity, empirical, 'Determinacy of the governing review method, the main driver of drift_state.').

omega_variable(
    preexisting_right_status,
    'Is the right this reading protects a pre-political natural right (which would give the arrangement mountain-like immunity from this seat) or a constructed entitlement revisable by ordinary constitutional process?',
    'Conceptual analysis of the reading''s own axioms under positivist and social-choice critique; test whether the deontological grounding survives without the natural-rights premise.',
    'Natural-right framing immunizes the arrangement from legislative revision and pushes computed classification toward mountain-like immunity despite enacted form; construct framing restores ordinary revision legitimacy and lowers persistence estimates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preexisting_right_status, conceptual, 'Whether the reading''s deontological axiom makes the arrangement natural-law-flavored or ordinary positive law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1791, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sam_individual_right_tr_t1791, second_amendment_scope__individual_right_reading, theater_ratio, 1791, 0.06).
narrative_ontology:measurement_basis(sam_individual_right_tr_t1791, observed).
narrative_ontology:measurement(sam_individual_right_tr_t1865, second_amendment_scope__individual_right_reading, theater_ratio, 1865, 0.55).
narrative_ontology:measurement_basis(sam_individual_right_tr_t1865, observed).
narrative_ontology:measurement(sam_individual_right_tr_t1934, second_amendment_scope__individual_right_reading, theater_ratio, 1934, 0.45).
narrative_ontology:measurement_basis(sam_individual_right_tr_t1934, observed).
narrative_ontology:measurement(sam_individual_right_tr_t1939, second_amendment_scope__individual_right_reading, theater_ratio, 1939, 0.6).
narrative_ontology:measurement_basis(sam_individual_right_tr_t1939, observed).
narrative_ontology:measurement(sam_individual_right_tr_t1977, second_amendment_scope__individual_right_reading, theater_ratio, 1977, 0.55).
narrative_ontology:measurement_basis(sam_individual_right_tr_t1977, observed).
narrative_ontology:measurement(sam_individual_right_tr_t1994, second_amendment_scope__individual_right_reading, theater_ratio, 1994, 0.4).
narrative_ontology:measurement_basis(sam_individual_right_tr_t1994, observed).
narrative_ontology:measurement(sam_individual_right_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(sam_individual_right_tr_t2008, observed).
narrative_ontology:measurement(sam_individual_right_tr_t2010, second_amendment_scope__individual_right_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement_basis(sam_individual_right_tr_t2010, observed).
narrative_ontology:measurement(sam_individual_right_tr_t2022, second_amendment_scope__individual_right_reading, theater_ratio, 2022, 0.11).
narrative_ontology:measurement_basis(sam_individual_right_tr_t2022, observed).
narrative_ontology:measurement(sam_individual_right_tr_t2025, second_amendment_scope__individual_right_reading, theater_ratio, 2025, 0.13).
narrative_ontology:measurement_basis(sam_individual_right_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(sam_individual_right_be_t1791, second_amendment_scope__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(sam_individual_right_be_t1791, observed).
narrative_ontology:measurement(sam_individual_right_be_t1865, second_amendment_scope__individual_right_reading, base_extractiveness, 1865, 0.14).
narrative_ontology:measurement_basis(sam_individual_right_be_t1865, observed).
narrative_ontology:measurement(sam_individual_right_be_t1934, second_amendment_scope__individual_right_reading, base_extractiveness, 1934, 0.17).
narrative_ontology:measurement_basis(sam_individual_right_be_t1934, observed).
narrative_ontology:measurement(sam_individual_right_be_t1939, second_amendment_scope__individual_right_reading, base_extractiveness, 1939, 0.15).
narrative_ontology:measurement_basis(sam_individual_right_be_t1939, observed).
narrative_ontology:measurement(sam_individual_right_be_t1977, second_amendment_scope__individual_right_reading, base_extractiveness, 1977, 0.18).
narrative_ontology:measurement_basis(sam_individual_right_be_t1977, observed).
narrative_ontology:measurement(sam_individual_right_be_t1994, second_amendment_scope__individual_right_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement_basis(sam_individual_right_be_t1994, observed).
narrative_ontology:measurement(sam_individual_right_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.27).
narrative_ontology:measurement_basis(sam_individual_right_be_t2008, observed).
narrative_ontology:measurement(sam_individual_right_be_t2010, second_amendment_scope__individual_right_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement_basis(sam_individual_right_be_t2010, observed).
narrative_ontology:measurement(sam_individual_right_be_t2022, second_amendment_scope__individual_right_reading, base_extractiveness, 2022, 0.34).
narrative_ontology:measurement_basis(sam_individual_right_be_t2022, observed).
narrative_ontology:measurement(sam_individual_right_be_t2025, second_amendment_scope__individual_right_reading, base_extractiveness, 2025, 0.35).
narrative_ontology:measurement_basis(sam_individual_right_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(sam_individual_right_su_t1791, second_amendment_scope__individual_right_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement_basis(sam_individual_right_su_t1791, observed).
narrative_ontology:measurement(sam_individual_right_su_t1865, second_amendment_scope__individual_right_reading, suppression_requirement, 1865, 0.2).
narrative_ontology:measurement_basis(sam_individual_right_su_t1865, observed).
narrative_ontology:measurement(sam_individual_right_su_t1934, second_amendment_scope__individual_right_reading, suppression_requirement, 1934, 0.22).
narrative_ontology:measurement_basis(sam_individual_right_su_t1934, observed).
narrative_ontology:measurement(sam_individual_right_su_t1939, second_amendment_scope__individual_right_reading, suppression_requirement, 1939, 0.12).
narrative_ontology:measurement_basis(sam_individual_right_su_t1939, observed).
narrative_ontology:measurement(sam_individual_right_su_t1977, second_amendment_scope__individual_right_reading, suppression_requirement, 1977, 0.14).
narrative_ontology:measurement_basis(sam_individual_right_su_t1977, observed).
narrative_ontology:measurement(sam_individual_right_su_t1994, second_amendment_scope__individual_right_reading, suppression_requirement, 1994, 0.2).
narrative_ontology:measurement_basis(sam_individual_right_su_t1994, observed).
narrative_ontology:measurement(sam_individual_right_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement_basis(sam_individual_right_su_t2008, observed).
narrative_ontology:measurement(sam_individual_right_su_t2010, second_amendment_scope__individual_right_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement_basis(sam_individual_right_su_t2010, observed).
narrative_ontology:measurement(sam_individual_right_su_t2022, second_amendment_scope__individual_right_reading, suppression_requirement, 2022, 0.72).
narrative_ontology:measurement_basis(sam_individual_right_su_t2022, observed).
narrative_ontology:measurement(sam_individual_right_su_t2025, second_amendment_scope__individual_right_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(sam_individual_right_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'the Second Amendment' — decomposes into three structurally distinct constraints (individual/civic/collective readings) with different beneficiary sets, victim sets, and epsilon over the same standing arrangement; family members link via affects_constraints. The individual-right reading is currently upstream in institutional terms: its judicial adoption (Heller through Bruen) changed the operating environment of the sibling readings, which persist as political positions and scholarly programs rather than governing doctrine. Epsilon differs across the family by design (OQ-26 reading-indexed values over a fixed referent): this reading authors low-moderate extraction; the collective reading is expected to author high extraction; the civic reading intermediate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
