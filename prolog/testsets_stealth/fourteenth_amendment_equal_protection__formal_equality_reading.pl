% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection — Formal Equality Reading (Anti-Classification Principle)
 *   domain: constitutional law/political philosophy/civil rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the
 *   formal_equality_reading of the Fourteenth Amendment Equal Protection
 *   guarantee, under which the clause prohibits explicit state racial and
 *   status classifications absent compelling justification. The standing
 *   arrangement under contest is the colorblind doctrinal regime as it
 *   currently operates — strict scrutiny for racial classifications, the
 *   discriminatory-intent requirement, and the progressive invalidation of
 *   race-conscious remedial programs. The sibling reading
 *   (anti_caste_reading, a separate constraint story) reads the same text as
 *   mandating active dismantling of hierarchy; per the epsilon-invariance
 *   principle the two are separate constraints with separate epsilon values,
 *   linked through network edges, not one constraint viewed two ways. KEY
 *   AGENTS (by structural relationship): federal_judiciary: agenda setter
 *   (institutional/constrained) — administers and enforces the rule;
 *   racial_minority_litigants: dual-positioned beneficiary/payer
 *   (organized/constrained) — protected by the rule's core, exposed by its
 *   extension; majority_group_applicants: principal collector of the rule's
 *   redistributive effect (moderate/mobile); public_universities: payer
 *   (institutional/constrained); minority_business_contractors: payer
 *   (moderate/constrained); structurally_disadvantaged_communities: payer and
 *   procedurally excluded voice (powerless/trapped);
 *   civil_rights_advocacy_organizations: beneficiary with identity-locked
 *   exit (organized); federal_congress: founding agenda setter turned
 *   constrained payer (institutional); constitutional_scholars: analytical
 *   observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.32).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.64).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection — Formal Equality Reading (Anti-Classification Principle)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional law/political philosophy/civil rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'be9df6d1-e431-4beb-8d04-41ec7794cb2f').
narrative_ontology:cs_kernel_codification('be9df6d1-e431-4beb-8d04-41ec7794cb2f', fixed_text).
narrative_ontology:cs_authority_grounding('be9df6d1-e431-4beb-8d04-41ec7794cb2f', lineage).
narrative_ontology:cs_interpretation_layer_present('be9df6d1-e431-4beb-8d04-41ec7794cb2f').
narrative_ontology:cs_reading_relation('be9df6d1-e431-4beb-8d04-41ec7794cb2f', fourteenth_amendment_equal_protection__anti_caste_reading, influences).
narrative_ontology:cs_axiom('be9df6d1-e431-4beb-8d04-41ec7794cb2f', foundational, racial_classifications_presumptively_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classifications_presumptively_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('be9df6d1-e431-4beb-8d04-41ec7794cb2f', racial_classifications_presumptively_unconstitutional, deontological).
narrative_ontology:cs_axiom('be9df6d1-e431-4beb-8d04-41ec7794cb2f', foundational, structural_inequality_is_preconstitutional_background).
narrative_ontology:cs_axiom_status(structural_inequality_is_preconstitutional_background, holdable).
narrative_ontology:cs_axiom_grounding('be9df6d1-e431-4beb-8d04-41ec7794cb2f', structural_inequality_is_preconstitutional_background, conventional).
narrative_ontology:cs_reference_frame('be9df6d1-e431-4beb-8d04-41ec7794cb2f', formal_equal_treatment_baseline).
narrative_ontology:cs_drift_state('be9df6d1-e431-4beb-8d04-41ec7794cb2f', contemporary_post_sffa_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('be9df6d1-e431-4beb-8d04-41ec7794cb2f', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_litigants).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, majority_group_applicants).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, public_universities).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, minority_business_contractors).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, structurally_disadvantaged_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_litigants).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, federal_congress).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, formal_neutrality_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, strict_scrutiny_framework).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalism).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, anti_classification_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, state_action_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the rule that state racial and status classifications require compelling justification. Decides which state programs survive, which are struck down, and how demanding the justification must be. Bound by the constitutional text, its own precedents, and confirmation politics; it cannot opt out of deciding these cases once they arrive, and it accumulates interpretive authority with every ruling.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Bring suits against explicitly discriminatory state action — segregation, biased administration, facially exclusionary laws — and win under this rule. The same rule, however, is now invoked against race-conscious programs designed for their communities' advancement, so the tool that protects them also dismantles measures built for them. They cannot leave the jurisdiction of the rule; their recourse is to argue within it.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_litigants, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_litigants, payer).

% Individual applicants and contractors not targeted by race-conscious programs who challenge those programs in court and, increasingly, prevail. When a university admissions plan or a contracting set-aside is struck down, the opportunities it allocated flow to them. They face no barrier to raising the claim, bear no cost from the rule's operation, and can pursue the same opportunities through ordinary channels regardless of outcome.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, majority_group_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Operate admissions systems that sought student-body diversity through race-conscious means, and have watched successive rulings narrow, then close, that path. Each ruling forces redesign of admissions machinery at significant administrative cost, and compliance is mandatory — a public university cannot decline the Constitution's application or relocate outside its reach.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, public_universities, payer,
    institutional, generational, constrained, national).

% Firms owned by members of minority groups that built pipelines into public contracting through set-aside and subcontracting programs. Judicial decisions applying strict scrutiny to such programs have invalidated or hollowed many of them, removing guaranteed participation shares these firms had structured their growth around. Their alternatives are competing in open bidding against incumbents with longer track records and deeper capital.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, minority_business_contractors, payer,
    moderate, biographical, constrained, regional).

% Communities carrying the cumulative weight of historical subordination — wealth gaps, school segregation traces, environmental exposure — whose disadvantage arises from patterns no single actor intended. The governing rule requires proof of discriminatory intent for a constitutional violation, so their circumstances generate no cognizable claim, and measures aimed at their condition are themselves vulnerable to challenge. They have no exit from the jurisdiction and no procedural seat from which to argue that neutrality itself maintains their position.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, structurally_disadvantaged_communities, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, structurally_disadvantaged_communities, excluded).

% Litigation organizations built over a century around enforcing equal-treatment guarantees. Their dockets, funding bases, staff expertise, and public identities are constituted through this body of law. They continue to win cases under it against explicit discrimination while simultaneously watching it invalidate the remedial programs they designed — and they cannot abandon the framework without dissolving the professional identity, donor relationships, and doctrinal expertise that define them.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocacy_organizations, payer).

% Proposed and ratified the Fourteenth Amendment and retains power to enforce it through legislation. Its Reconstruction-era enforcement included expressly race-conscious protective measures; its modern corrective statutes are reviewed against the same demanding standard as state action, and several enforcement regimes have been narrowed or invalidated. It set the agenda at the founding and now finds its contemporary exercises of that agenda subject to the rule it created.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_congress, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, federal_congress, payer).

% Academics, historians, and commentators who map the doctrine's evolution, reconstruct the framing generation's understanding, and articulate competing readings of the text. They bear no costs and collect no allocations; their output shapes the argumentative environment in which judges, litigants, and legislators operate.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, majority_group_applicants).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, judicially enforceable baseline against state racial and status discrimination, solving a genuine collective-action problem: without it, majority factions in each state could capture state law to subordinate minorities, and citizens would hold no stable expectation of equal treatment before state power. It also gives legislatures and administrators a predictable boundary on permissible classification.
% TRANSFER_FUNCTION: Moves adjudicative authority over race-conscious state action from legislatures and administrators to courts; moves educational seats, contract awards, and program resources away from race-targeted allocation toward untargeted allocation; and shifts the burden of justifying any classification onto the state acting.
% ABSENT_VOICES: Anti-subordination theorists and the communities experiencing structural inequality would object that neutrality administered atop entrenched hierarchy reproduces the hierarchy — their framework appears in the record only as dissent and scholarship, never as governing doctrine. The Reconstruction generation's own practice, which included expressly race-conscious protective legislation, is likewise displaced from the conversation about what the text permits.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, the entire civil-rights litigation infrastructure would lose its operative standard: challenges to explicit discrimination would proceed under scattered statutory and state-law hooks, university admissions and public contracting would reorganize immediately, and states would regain unrestricted power to classify — a rearrangement touching every institution the doctrine touches daily.
% FOUNDING_PROBLEM: The failure of Reconstruction on the ground: newly freed citizens facing Black Codes, state-sponsored caste, and official indifference or hostility — the Fourteenth Amendment was written to give federal constitutional force against state-imposed subordination.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any current beneficiary set: the legislative record of the 39th Congress, Freedmen's Bureau correspondence, and the historiography of Reconstruction document the founding problem directly; the continuing stream of successful litigation against explicit state discrimination attests that at least the original core of the problem persists. No party disputes that the founding problem existed; the parties dispute whether it persists in a form this rule still addresses.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.32 as THIS reading assesses the standing arrangement by its own lights: the regime is, in the formal-equality frame, protective coordination — it prevents the gravest state abuses and the burdens it places on race-conscious remedies are counted by the reading as required consistency, not as wrongful taking. The referent is the existing colorblind regime, never the anti-caste alternative this reading declines to endorse. Suppression is authored at 0.64 as a RAW structural property, unscaled by power or scope: the regime's persistence depends on active judicial nullification of rival frameworks (impact-based theories, caste-analysis arguments, race-conscious legislation), and the suppression_requirement series documents an enforcement ratchet — fragmented plurality reasoning at t0 (Bakke, 1978), strict scrutiny extended to set-asides at t11 (Croson, 1989), congruence extended to federal programs at t17 (Adarand, 1995), a partial reprieve at t25 (Grutter, 2003), diversity rationale narrowed at t29 (Parents Involved, 2007), narrow-tailoring hardened at t38 (Fisher II, 2016), and consolidation of colorblindness at t45 (SFFA, 2023). Theater_ratio rises 0.16 to 0.33 as compelling-interest balancing grows more outcome-driven and narrow-tailoring analysis more formulaic, while the core anti-discrimination function remains fully operational — hence a moderate, not degraded, value. Accessibility_collapse sits at 0.45 because alternatives do not vanish: the anti-caste framework survives in the academy, in dissenting opinions, in state constitutional provisions, and in legislative proposals. Resistance at 0.60 reflects sustained scholarly critique, recurring dissents, and legislative counter-movement. All three series share one time grid (t = 0, 11, 17, 25, 29, 38, 45) so every metric is authored at every examined point; the single Grutter-era dip in extractiveness is a policy-window artifact, not an oscillation cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (federal_judiciary), the arrangement presents as a neutral principle faithfully administered — the experience is of protective coordination it stewards. From the payer seats (public_universities, minority_business_contractors), the same structure presents as a binding asymmetry: their chosen instruments are struck down while the underlying distributions that made those instruments necessary remain untouched. The dual-positioned seat (racial_minority_litigants) splits across its own roles — winning as beneficiary in explicit-discrimination cases while losing as payer in remedial-program cases — so its computed classification depends on which structural relationship dominates the derivation. The excluded seat (structurally_disadvantaged_communities) experiences the arrangement least as a shield and most as a closed door, but contributes no enforcement pressure because its objections are screened at the intent requirement. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: racial_minority_litigants (core protection), majority_group_applicants (collect the redistributed opportunities), civil_rights_advocacy_organizations (collect wins and institutional purpose). Victims declared: public_universities (forced redesign at cost), minority_business_contractors (lost guaranteed participation), structurally_disadvantaged_communities (conditions rendered non-cognizable). The derivation chain places declared beneficiaries near the subsidized end and declared victims near the full-target end, modulated by exit: majority_group_applicants carry mobile exit and sit nearest the beneficiary pole; trapped communities sit nearest the target pole despite receiving nominal formal protection. Dual-role agents derive intermediate directionality from their paired declarations. The judiciary derives near-symmetric: it pays little and collects no material allocation, though interpretive authority accrues to it — receipt of authority is recorded on the receipt surface, not in the beneficiary array. Vindicated propositions (formal_neutrality_doctrine, strict_scrutiny_framework, colorblind_constitutionalism, anti_classification_principle, state_action_doctrine) are listed separately: they collect no rents and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-imposed caste — has transformed rather than died: explicit state discrimination persists and is successfully challenged, while the center of gravity of racial disadvantage has shifted toward structures the intent requirement renders invisible. Hence founding_problem_status is contested, not dead, and no zombie flag is warranted. The tangled_rope claim prevents two symmetrical misreadings: reading the arrangement as pure coordination (rope) would erase the documented asymmetry that entered when state corrective action joined the set of things the rule strikes down; reading it as pure extraction (snare) would erase the genuine, heavily used anti-discrimination function that thousands of successful litigations attest. The rising theater_ratio signals narrowing of the compelling-interest inquiry toward performance, but the functional core remains primary — this is not yet a piton, and the cost-asymmetry test confirms it: the judiciary could shift readings, but the systemic cost of reversal (reliance interests, institutional legitimacy, destabilized admissions and contracting sectors) vastly exceeds what the administrator itself bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the fourteenth_amendment_equal_protection kernel; what structurally changes if the anti_caste_reading were adopted instead?',
    'Comparative analysis across the two sibling stories: the victim set shifts from race-conscious programs to subordinated groups, corrective legislation moves from target to instrument, and the epsilon authored over the same referent rises substantially under the sibling''s lights.',
    'Classification of the same constitutional arrangement flips between stories rather than within one — the corpus models the contest as two linked constraints, and any verdict about ''equal protection'' generally must cite both files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame omega recording that this story instantiates one reading of a contested kernel and naming the sibling.').

omega_variable(
    original_understanding_colorblind_vs_protective,
    'Does the framing generation''s own understanding support colorblindness, or did the 39th Congress contemplate and enact race-conscious protective measures as consistent with the amendment?',
    'Historiographic and archival resolution: systematic analysis of Reconstruction legislative records, Freedmen''s Bureau enabling statutes, and early enforcement legislation against the interpretive claims of both readings.',
    'If the original understanding accommodates race-conscious protection, the formal reading loses its lineage grounding and its authority rests on later doctrinal construction; if colorblindness is original, the anti-caste reading rests on living-constitution premises alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_understanding_colorblind_vs_protective, empirical, 'Whether the founding record supports this reading''s claim to lineage.').

omega_variable(
    intent_requirement_screening_severity,
    'What proportion of structurally-produced racial disadvantage is screened out of constitutional cognizance by the discriminatory-intent requirement?',
    'Empirical study of claim attrition: compare the population of documented racial disparities in state-administered domains against the subset generating viable constitutional claims under the intent standard.',
    'Severe screening deepens the arrangement''s asymmetry — protection concentrated on explicit bias while structural disadvantage generates no claim — and pushes the payer seats'' computed classifications toward harder types; mild screening supports the reading''s own protective framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_requirement_screening_severity, empirical, 'How much structural disadvantage the intent requirement renders non-cognizable.').

omega_variable(
    neutrality_entrenchment_or_equilibrium,
    'Does formal neutrality administered atop entrenched hierarchy reproduce the hierarchy (the anti-caste charge), or does it constitute the only workable long-run equilibrium for a multiethnic polity (this reading''s reply)?',
    'Not resolvable by data alone: depends on prior commitments about whether distributive baselines are normatively neutral. Comparative evidence from jurisdictions adopting each approach (race-conscious versus strictly neutral remediation) can constrain, but not settle, the question.',
    'If neutrality entrenches, this reading''s coordination function is partly cover and its effective extraction is understated at 0.32; if neutrality equilibrates, the anti-caste sibling''s higher epsilon reflects a contested evaluative frame rather than a structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(neutrality_entrenchment_or_equilibrium, conceptual, 'The central contest between the sibling readings, routed here as an irreducible uncertainty.').

omega_variable(
    compelling_interest_escape_hatch_stability,
    'Will the compelling-interest/narrow-tailoring escape hatch remain genuinely open, keeping the prohibition soft for narrowly tailored uses, or will continued narrowing close it?',
    'Track post-SFFA litigation: whether military academies, remedial contexts, or new compelling interests sustain any race-conscious classification, and how strictly narrow-tailoring is policed.',
    'A closing hatch converts the remaining coordination-side flexibility into pure prohibition, hardening the arrangement for institutional payers and accelerating the theater_ratio rise; a stable hatch preserves the hybrid character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_escape_hatch_stability, empirical, 'Whether the prohibition''s internal flexibility survives current doctrinal trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(four_tr_t0, observed).
narrative_ontology:measurement(four_tr_t11, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 11, 0.19).
narrative_ontology:measurement_basis(four_tr_t11, observed).
narrative_ontology:measurement(four_tr_t17, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 17, 0.21).
narrative_ontology:measurement_basis(four_tr_t17, observed).
narrative_ontology:measurement(four_tr_t25, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(four_tr_t25, observed).
narrative_ontology:measurement(four_tr_t29, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 29, 0.28).
narrative_ontology:measurement_basis(four_tr_t29, observed).
narrative_ontology:measurement(four_tr_t38, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 38, 0.3).
narrative_ontology:measurement_basis(four_tr_t38, observed).
narrative_ontology:measurement(four_tr_t45, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement_basis(four_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(four_be_t0, observed).
narrative_ontology:measurement(four_be_t11, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 11, 0.27).
narrative_ontology:measurement_basis(four_be_t11, observed).
narrative_ontology:measurement(four_be_t17, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 17, 0.3).
narrative_ontology:measurement_basis(four_be_t17, observed).
narrative_ontology:measurement(four_be_t25, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 25, 0.29).
narrative_ontology:measurement_basis(four_be_t25, observed).
narrative_ontology:measurement(four_be_t29, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 29, 0.31).
narrative_ontology:measurement_basis(four_be_t29, observed).
narrative_ontology:measurement(four_be_t38, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 38, 0.32).
narrative_ontology:measurement_basis(four_be_t38, observed).
narrative_ontology:measurement(four_be_t45, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 45, 0.32).
narrative_ontology:measurement_basis(four_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(four_su_t0, observed).
narrative_ontology:measurement(four_su_t11, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 11, 0.47).
narrative_ontology:measurement_basis(four_su_t11, observed).
narrative_ontology:measurement(four_su_t17, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 17, 0.53).
narrative_ontology:measurement_basis(four_su_t17, observed).
narrative_ontology:measurement(four_su_t25, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(four_su_t25, observed).
narrative_ontology:measurement(four_su_t29, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 29, 0.59).
narrative_ontology:measurement_basis(four_su_t29, observed).
narrative_ontology:measurement(four_su_t38, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 38, 0.61).
narrative_ontology:measurement_basis(four_su_t38, observed).
narrative_ontology:measurement(four_su_t45, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 45, 0.64).
narrative_ontology:measurement_basis(four_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Equal Protection' covers two structurally distinct constraints held by different parties. This file instantiates the formal_equality_reading (clause prohibits classification; structural inequality is pre-constitutional background; corrective state action joins the target set; low epsilon by this reading's own lights). The sibling file instantiates the anti_caste_reading (clause mandates dismantling hierarchy; corrective action is the instrument, not the target; substantially higher epsilon over the same referent). The upstream/downstream relation runs from this reading to the sibling: each extension of colorblind doctrine narrows the sibling's institutional space without logically eliminating it (Grutter demonstrated twenty years of coexistence under strict scrutiny). Both files link each other through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
