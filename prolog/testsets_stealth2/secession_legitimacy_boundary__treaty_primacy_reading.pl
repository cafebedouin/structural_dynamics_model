% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Secession Legitimacy Boundary — Standing Two-Level Framework (Treaty Primacy Reading)
 *   domain: political/constitutional/resource-politics
 *
 * SUMMARY:
 *   The kernel 'secession_legitimacy_boundary' — the rule governing when a
 *   unit's exit from a federation is legitimate — is contested by four
 *   readings. This file instantiates the treaty_primacy_reading: Indigenous
 *   treaty rights predate and supersede both federal and provincial
 *   authority, and no secession is legitimate without treaty-holder consent.
 *   Per the epsilon-referent rule for kernel readings, extractiveness is
 *   authored for the STANDING arrangement under contest — the post-1998
 *   Reference framework that constitutes secession legitimacy as a matter
 *   between the two orders of government, giving treaty nations consultation
 *   rights but no consent right over territorial rearrangement — assessed by
 *   this reading's own lights. The reading sees that arrangement as a
 *   standing transfer of decision rights over treaty territories to the two
 *   governments and the separatist project, sustained by Crown-sovereignty
 *   doctrine. Claim and metrics are independent: claimed_type is tangled_rope
 *   (the framework genuinely coordinates the two governments against rupture
 *   AND asymmetrically excludes the third sovereign from the consent
 *   decision), while the metrics describe extraction-heavy operation; the
 *   engine computes per-seat classifications from the structural data.
 *   Interval mapping: T0 approximates 1995 (the referendum near-miss that
 *   posed the founding problem), T30 approximates 2025. KEY AGENTS (by
 *   structural relationship): federal_government — primary beneficiary and
 *   co-agenda-setter (institutional/constrained); supreme_court_canada —
 *   agenda-setter (institutional/constrained); provincial_governments —
 *   beneficiary (institutional/constrained); separatist_provincial_movements
 *   — beneficiary (organized/constrained); provincial_secession_electorates —
 *   beneficiary (organized/constrained); resource_extraction_corporations —
 *   beneficiary (powerful/arbitrage); indigenous_treaty_nations — primary
 *   payer (organized/trapped); unceded_territory_nations — payer
 *   (organized/trapped); international_law_bodies — excluded
 *   (moderate/mobile).
 *
 * KEY AGENTS:
 *   - federal_government: primary beneficiary and co-agenda-setter (institutional/constrained) — holds the sole federal treaty-partner seat and negotiation primacy over any territorial rearrangement
 *   - supreme_court_canada: agenda-setter (institutional/constrained) — authored the boundary doctrine in the 1998 Reference and administers its content
 *   - provincial_governments: beneficiary (institutional/constrained) — hold resource jurisdiction and the referendum trigger
 *   - separatist_provincial_movements: beneficiary (organized/constrained) — pursue exit priced without treaty-holder consent
 *   - provincial_secession_electorates: beneficiary (organized/constrained) — hold the vote that triggers negotiation but not self-legitimating exit
 *   - resource_extraction_corporations: beneficiary (powerful/arbitrage) — permitting security on treaty lands without a third-sovereign veto; strongest exit in the story
 *   - indigenous_treaty_nations: primary payer (organized/trapped) — territories are the object of any rearrangement; consultation without consent; no exit from the order
 *   - unceded_territory_nations: payer (organized/trapped) — lack even a treaty instrument to invoke; litigation is the sanctioned channel
 *   - international_law_bodies: excluded (moderate/mobile) — hold the free-prior-informed-consent standard, seated nowhere in the domestic framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.78).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.72).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Secession Legitimacy Boundary — Standing Two-Level Framework (Treaty Primacy Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political/constitutional/resource-politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, 'c90e95ea-2963-4227-86d1-3f2702fdd3c0').
narrative_ontology:cs_kernel_codification('c90e95ea-2963-4227-86d1-3f2702fdd3c0', formalized).
narrative_ontology:cs_authority_grounding('c90e95ea-2963-4227-86d1-3f2702fdd3c0', extraction).
narrative_ontology:cs_interpretation_layer_present('c90e95ea-2963-4227-86d1-3f2702fdd3c0').
narrative_ontology:cs_reading_relation('c90e95ea-2963-4227-86d1-3f2702fdd3c0', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('c90e95ea-2963-4227-86d1-3f2702fdd3c0', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('c90e95ea-2963-4227-86d1-3f2702fdd3c0', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('c90e95ea-2963-4227-86d1-3f2702fdd3c0', foundational, treaty_primacy_over_settler_authority).
narrative_ontology:cs_axiom_status(treaty_primacy_over_settler_authority, holdable).
narrative_ontology:cs_axiom_grounding('c90e95ea-2963-4227-86d1-3f2702fdd3c0', treaty_primacy_over_settler_authority, deontological).
narrative_ontology:cs_axiom('c90e95ea-2963-4227-86d1-3f2702fdd3c0', foundational, secession_illegitimate_without_treaty_consent).
narrative_ontology:cs_axiom_status(secession_illegitimate_without_treaty_consent, holdable).
narrative_ontology:cs_axiom_grounding('c90e95ea-2963-4227-86d1-3f2702fdd3c0', secession_illegitimate_without_treaty_consent, deontological).
narrative_ontology:cs_axiom('c90e95ea-2963-4227-86d1-3f2702fdd3c0', secondary, consultation_is_not_consent).
narrative_ontology:cs_axiom_status(consultation_is_not_consent, holdable).
narrative_ontology:cs_axiom_grounding('c90e95ea-2963-4227-86d1-3f2702fdd3c0', consultation_is_not_consent, deontological).
narrative_ontology:cs_reference_frame('c90e95ea-2963-4227-86d1-3f2702fdd3c0', nation_to_nation_treaty_federalism).
narrative_ontology:cs_drift_state('c90e95ea-2963-4227-86d1-3f2702fdd3c0', post_reference_re_secession_doctrine, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c90e95ea-2963-4227-86d1-3f2702fdd3c0', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, separatist_provincial_movements).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secession_electorates).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, resource_extraction_corporations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, unceded_territory_nations).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, reference_re_secession_negotiation_framework).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, provincial_resource_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the treaty relationship as the senior order of government and the sole federal treaty partner. Co-authored the secession framework after the 1995 Quebec referendum and controls the negotiation seat any territorial rearrangement runs through. Collects territorial integrity and negotiation primacy; cannot unilaterally alter entrenched treaty rights and must carry provincial governments along. Exit would mean repudiating the constitutional order it administers.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, agenda_setter).

% Authored the secession-legitimacy framework in the 1998 Reference and continues to define its content through treaty-rights and consultation rulings. Every dispute over the boundary arrives at its docket; it revises the framework only through incremental doctrine, since repudiating its own precedent would cost it the authority the framework rests on.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, supreme_court_canada, agenda_setter,
    institutional, generational, constrained, national).

% Administer Crown lands, natural resources, and permitting within their borders, and own the referendum trigger that starts any secession process. They collect resource jurisdiction and agenda control; when treaty nations assert jurisdiction over the same lands they face litigation and blockades, but the consent decision over territorial rearrangement never moves to the nations.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, beneficiary,
    institutional, generational, constrained, regional).

% Organize to carry a province out of the federation by referendum and negotiation. The standing framework prices their project without any treaty-holder consent requirement — the cheapest legitimacy path on offer among the readings of the boundary. They are bound to the provincial territory whose borders they seek to redraw.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, separatist_provincial_movements, beneficiary,
    organized, generational, constrained, regional).

% A clear referendum majority obligates the two governments to negotiate under the standing framework. Their vote starts the process but does not by itself legitimate the exit; they bear the costs of prolonged negotiation and of any concessions won from them along the way, and they cannot relocate away from the consequences.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secession_electorates, beneficiary,
    organized, biographical, constrained, regional).

% Build mines, pipelines, and transmission corridors under provincial permits across treaty territories. The standing boundary keeps the consent decision away from the nations whose lands host the corridors, which lowers project risk; when blockades or litigation raise costs, capital can move to other jurisdictions — the strongest exit position in the story.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, resource_extraction_corporations, beneficiary,
    powerful, biographical, arbitrage, continental).

% Hold historic and numbered treaties made with the Crown before either level of government existed in its present form. Their territories are the object of any secession rearrangement; the standing framework gives them consultation rights but no consent right over it. If a province left without their agreement they would wake inside a successor state with their treaties subordinated to its doctrine. They cannot exit the constitutional order — no international decolonization pathway is open to domestic treaty nations and their economies are integrated with the Canadian state.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations, payer,
    organized, generational, trapped, national).

% Never made treaties ceding their territories, or hold unextinguished title claims. The standing framework treats their lands as provincial and federal assets a fortiori — they lack even a treaty instrument to invoke. Title litigation is the sanctioned channel and runs for decades; direct assertion of jurisdiction meets injunctions and police enforcement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, unceded_territory_nations, payer,
    organized, generational, trapped, regional).

% UN treaty-monitoring bodies and the UNDRIP consent standard hold that decisions affecting Indigenous territories require free, prior and informed consent. They are seated nowhere in the domestic framework: their observations carry no enforcement inside the boundary, and the framework's architects never gave them a seat.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_law_bodies, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels secession politics between the two levels of government into a defined legal process: a clear referendum majority on a clear question obligates negotiation instead of permitting unilateral exit or rupture, and preserves a single federal treaty partner throughout.
% TRANSFER_FUNCTION: Moves decision rights over treaty territories and the terms of any territorial rearrangement to the federal and provincial governments; moves project security on treaty lands to resource capital; moves the costs of rearrangement — subordinated treaties, lost jurisdictional continuity, imposed successor doctrine — to treaty nations, who hold no consent right in the process.
% ABSENT_VOICES: Treaty nations are present as consultees but absent from the consent decision — the framework gives them voice without a gate, so the seat that would object to the boundary's constitution is in the room without power over it. International law bodies holding the free-prior-informed-consent standard would object that the framework's consultation floor sits below the international consent standard; they are seated nowhere. Unceded nations would object that consultation presupposes the jurisdictional subordination they contest.
% DISAPPEARANCE_RATIONALE: Without the framework, secession politics moves to unilateral-declaration attempts and intergovernmental force; the two governments lose their negotiation channel; treaty nations lose even the consultative floor they now hold; resource permitting on treaty lands collapses into open jurisdictional conflict. Every seat's arrangements depend on some boundary existing — the parties dispute only its content.
% FOUNDING_PROBLEM: The near-run 1995 Quebec referendum: how a federation handles a unit's attempt to leave without unilateral secession, violent rupture, or state fragmentation — answered by the 1998 Reference with a negotiated-exit framework built for two orders of government.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholarship outside the benefiting parties corroborates that the framework was constructed in response to the 1995 referendum near-miss. Treaty-nation legal scholarship and UN treaty-monitoring observations corroborate from outside the beneficiary set that the problem is live but was constituted at founding without the third sovereign. No beneficiary-only source is relied on.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the referent is the standing arrangement assessed by this reading's lights — the entire consent decision over treaty territories is allocated to the two levels of government; recognition jurisprudence (title declaration, consultation duty) raised the price of individual expropriations but never moved the consent boundary itself, while resource-race dynamics on treaty territories increased what the closed boundary protects. Suppression 0.72 is a raw structural property (unscaled by power or scope): Crown-sovereignty doctrine, deferential treaty interpretation, no international decolonization pathway for domestic nations, and injunctions with police enforcement against jurisdiction assertions. Theater_ratio 0.48: land acknowledgments, reconciliation rhetoric, and UNDRIP declarations coexist with an unchanged consent boundary; the rising series tracks the widening gap between recognition performance and boundary content. Accessibility_collapse 0.6: exit alternatives for treaty nations largely collapse once the boundary is understood (no international pathway, total economic integration), but direct action, litigation, and international advocacy remain partly open. Resistance 0.65: blockades, title litigation, and jurisdiction assertions require continuous active defense of the boundary. Receipt surface: gains land primarily on the federal seat (sole treaty partner, negotiation primacy, territorial integrity) with provincial governments capturing the resource-jurisdiction share — 'diffuse' is not authored because two named seats demonstrably capture. Fixing cost: replacing the boundary with a treaty-consent gate requires constitutional amendment across entrenchment clauses plus the Court repudiating its own framework — prohibitive relative to the legitimacy benefit. All three metric series run on one shared six-point grid.
 *
 * PERSPECTIVAL GAP:
 *   From the federal, provincial, and separatist seats the boundary is legitimate constitutional order — coordination that prevents rupture, experienced as rope-like. From the treaty-nation seats the same structure operates as enforced exclusion from the consent decision over their own territories — snare-like. The resource-corporation seat experiences it as permitting stability; the excluded international seat experiences it as a consent-standard violation with no domestic lever. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The five beneficiary declarations map to seats near the beneficiary end of d: the boundary subsidizes their decision rights, agenda control, and project security. Both payer groups are trapped — they cannot exit the constitutional order whose boundary allocates their territories — which drives them toward the full-target end, so effective extraction lands near full epsilon for them while staying damped for mobile resource capital. The federal government's position is genuinely dual (collects primacy; bears entrenchment constraints and fiduciary obligations), captured by its secondary agenda-setter role rather than a directionality override: the override mechanism keys on power atom, and the institutional atom spans seats with genuinely different relationships, so an override would distort the provincial and judicial seats it cannot distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. A snare label would erase the genuine two-level coordination function: a legitimacy process that channels secession away from unilateral declaration and force has real value for every seat, including treaty nations, relative to the violent alternative. A rope label would erase the asymmetric extraction: the consent gap is a standing transfer of decision rights from treaty holders to the two governments, enforced by courts and police power. On obsolescence: the founding problem (channeling secession away from rupture) remains live — separatist politics persist — so the boundary has not outlived its function in the two-level sense; but this reading's point is that the mandate was misconceived at founding: a two-party process for a three-party territory. The classification keeps that critique visible without collapsing the boundary into pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the treaty_primacy_reading of kernel secession_legitimacy_boundary — what would each sibling reading change structurally if adopted in place of it?',
    'Compare the four readings'' consent gates and victim sets: popular_sovereignty_reading deletes the Indigenous consent gate entirely (provincial referendum self-legitimating); constitutional_impossibility_reading relocates the gate to constitutional-amendment procedure with no treaty-holder seat; grievance_threshold_reading replaces the fixed gate with a variable structural-injustice threshold; this reading fixes the gate in treaty-holder consent.',
    'The disagreement is located in the locus of ultimate sovereignty over treaty territories. Adopting a sibling changes who holds the consent right, hence the victim set: Indigenous nations are victims of the standing arrangement precisely because it lacks the gate this reading would install.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of the secession-legitimacy kernel; sibling readings relocate or delete the consent gate.').

omega_variable(
    consent_vs_consultation_substitution,
    'Is duty-to-consult jurisprudence a partial drift toward treaty-holder consent, or a procedural substitute that entrenches the standing boundary while performing recognition?',
    'Track whether courts ever condition territorial rearrangement itself (as opposed to individual project approvals) on consent; compare consultation outcomes against treaty-nation consent rates over the interval.',
    'If substitution, the theater_ratio understates the boundary''s performative layer and the boundary is stable; if drift toward consent, the standing arrangement is transitioning and the extractiveness series should be read as declining toward the reading''s frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_consultation_substitution, empirical, 'Whether consultation doctrine substitutes for, or drifts toward, the consent standard.').

omega_variable(
    victim_harm_conditionality,
    'Is the harm to treaty nations under the standing arrangement ongoing structural subordination, or contingent harm realized only if a secession actually proceeds without consent?',
    'Comparative analysis of territorial rearrangements with and without Indigenous consent, plus continuous measurement of jurisdictional exclusion between secession events.',
    'If contingent, the standing arrangement''s extractiveness is largely option-value held in reserve and realized ε is lower than authored; if ongoing, extraction is continuous and 0.78 understates it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_harm_conditionality, empirical, 'Whether the victim set''s harm is continuous or secession-triggered.').

omega_variable(
    boundary_vs_sovereignty_framing,
    'Is the standing arrangement under contest best framed narrowly as the Reference-era secession-legitimacy framework, or broadly as the Crown-sovereignty structure that subordinates all treaty relationships?',
    'Test epsilon stability across framings: if the narrowly framed boundary and the broadly framed sovereignty structure yield the same beneficiary/victim structure and epsilon, the narrow framing is adequate; if they diverge, decompose into two linked stories per the epsilon-invariance rule.',
    'The narrow framing authored here yields epsilon 0.78 scoped to the secession question; the broad framing would raise epsilon (the entire treaty relationship is extractive by the reading''s lights) and alter the authority analysis; all four kernel readings presuppose the narrow framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_vs_sovereignty_framing, conceptual, 'Framing under-determination: secession boundary versus Crown-sovereignty structure as the epsilon referent.').

omega_variable(
    enforcement_ratchet_vs_destabilization,
    'Does the rising suppression_requirement series indicate the boundary''s enforcement ratcheting toward stability, or mounting resistance destabilizing the boundary toward the treaty-primacy frame?',
    'Correlate resistance events (blockades, title-litigation wins, jurisdiction assertions) with subsequent doctrinal change; if enforcement escalations precede doctrinal concessions, the series reads as destabilization rather than ratchet.',
    'The ratchet reading supports the standing arrangement''s persistence as an extraction-heavy hybrid; the destabilization reading predicts drift toward this reading''s reference frame and a falling suppression series.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_ratchet_vs_destabilization, empirical, 'Whether rising enforcement signals entrenchment or impending boundary failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sece_tr_t6, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(sece_tr_t18, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sece_be_t6, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(sece_be_t18, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sece_su_t6, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(sece_su_t18, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'secession legitimacy' covers four structurally distinct claims about who must consent to territorial exit. This file is the treaty-primacy member; each sibling is authored separately with its own epsilon, beneficiaries, and victims, per the epsilon-invariance principle. The upstream member by empirical confidence is constitutional_impossibility_reading (the Reference framework is settled doctrine); the treaty-primacy reading contests its completeness, and the popular-sovereignty reading contests its premise. Edges here link this reading to all three siblings for contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
