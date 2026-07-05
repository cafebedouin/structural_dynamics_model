% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitution Reading — Evolving Meaning Doctrine
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the living-constitutionalism reading of the US
 *   Constitution kernel: the claim that constitutional meaning legitimately
 *   evolves with society, and that the 1787 text functions as an aspirational
 *   framework rather than a fixed code to be applied as originally
 *   understood. This is ONE reading among the declared set (living,
 *   originalist, positivist); the other two are separate constraint stories
 *   with their own ε values and stakeholder structures, linked via
 *   network.affects_constraints. The living reading has a genuine
 *   coordination function — adapting a deliberately hard-to-amend document to
 *   changed circumstances — but that function is bundled with a
 *   redistribution of interpretive authority from legislatures to courts and
 *   an asymmetric extraction from parties who lose in doctrinally-driven
 *   adjudication what they might have preserved through the amendment process
 *   or ordinary legislation.
 *
 * KEY AGENTS:
 *   - federal_judiciary: institutional agenda-setter administering the doctrine case by case
 *   - modern_rights_claimants: organized beneficiaries obtaining protection without needing Article V supermajorities
 *   - legislative_majorities: powerful payers whose enacted preferences are overridden by doctrinally-derived rulings
 *   - originalist_litigants: moderate-power payers whose textual/historical arguments are discounted
 *   - constitutional_law_academy: organized beneficiaries whose professional standing rides on the doctrine's continued generativity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.42).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.38).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitution Reading — Evolving Meaning Doctrine").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '0bb0872e-339e-4f68-a657-bc90f2dec3eb').
narrative_ontology:cs_kernel_codification('0bb0872e-339e-4f68-a657-bc90f2dec3eb', fixed_text).
narrative_ontology:cs_authority_grounding('0bb0872e-339e-4f68-a657-bc90f2dec3eb', practice).
narrative_ontology:cs_interpretation_layer_present('0bb0872e-339e-4f68-a657-bc90f2dec3eb').
narrative_ontology:cs_reading_relation('0bb0872e-339e-4f68-a657-bc90f2dec3eb', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bb0872e-339e-4f68-a657-bc90f2dec3eb', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('0bb0872e-339e-4f68-a657-bc90f2dec3eb', foundational, constitutional_meaning_tracks_social_evolution).
narrative_ontology:cs_axiom_status(constitutional_meaning_tracks_social_evolution, holdable).
narrative_ontology:cs_axiom_grounding('0bb0872e-339e-4f68-a657-bc90f2dec3eb', constitutional_meaning_tracks_social_evolution, instrumental).
narrative_ontology:cs_axiom('0bb0872e-339e-4f68-a657-bc90f2dec3eb', foundational, text_functions_as_aspirational_framework_not_binding_code).
narrative_ontology:cs_axiom_status(text_functions_as_aspirational_framework_not_binding_code, holdable).
narrative_ontology:cs_axiom_grounding('0bb0872e-339e-4f68-a657-bc90f2dec3eb', text_functions_as_aspirational_framework_not_binding_code, conventional).
narrative_ontology:cs_reference_frame('0bb0872e-339e-4f68-a657-bc90f2dec3eb', adaptive_common_law_constitutionalism).
narrative_ontology:cs_drift_state('0bb0872e-339e-4f68-a657-bc90f2dec3eb', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0bb0872e-339e-4f68-a657-bc90f2dec3eb', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, modern_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, constitutional_law_academy).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_litigants).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_governments_seeking_deference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal courts, especially the Supreme Court, apply the living-constitution frame to extend or find new constitutional rights (privacy, dignity, substantive due process) without requiring a textual amendment. They administer the doctrine's application case by case, choosing when 'evolving standards' license new holdings. They bear no direct cost from expanding the doctrine and gain interpretive authority and institutional relevance from it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Groups seeking recognition of rights not enumerated in 1787 text (reproductive autonomy, marriage equality, digital privacy) obtain constitutional protection through living-reading doctrine that would otherwise require the much harder Article V amendment process. They receive durable-seeming protection without needing supermajority political consensus.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, modern_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Legal scholars, especially those specializing in doctrinal theory, sustain careers producing frameworks (tiers of scrutiny, penumbras, evolving standards of decency) that legitimate and elaborate the living-reading approach. Their professional standing depends partly on the doctrine remaining contested and generative rather than settled.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_law_academy, beneficiary,
    organized, generational, mobile, national).

% Elected majorities that might prefer to resolve a contested social question through statute or amendment instead find courts have already settled it under the living-reading frame, foreclosing the ordinary democratic process. They can attempt to legislate around a ruling or pursue constitutional amendment, both high-cost and slow relative to the speed of doctrinal change.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, legislative_majorities, payer,
    powerful, biographical, constrained, national).

% Parties who structure arguments around original public meaning find courts applying the living-reading frame instead, discounting their textual and historical evidence in favor of contemporary-values reasoning. Their exit is limited to appeal within the same system or advocacy for judicial appointments favoring different interpretive methodology, a multi-decade project.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_litigants, payer,
    moderate, biographical, constrained, national).

% States that enacted laws reflecting local democratic consensus (on marriage, policing, abortion regulation, and similar contested areas) find those laws invalidated when federal courts identify a newly recognized constitutional right under the living-reading approach. Their recourse is limited to narrow statutory workarounds or waiting for doctrinal reversal.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_governments_seeking_deference, payer,
    institutional, generational, constrained, national).

% The 1787 text and its amendments are treated as a starting framework rather than a binding constraint on outcomes; the literal words recede from adjudicative force as courts prioritize the values the text is read to embody. Not an agent, but its diminished operational authority under this reading is the structural fact the doctrine turns on.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_text, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_1787__living_reading, constitutional_text).

% Later legal historians and theorists evaluate whether the living-reading approach produced genuine adaptation to social change or functioned as a vehicle for judicial policymaking insulated from democratic correction. They assess the doctrine's track record across political and ideological cycles.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, future_constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows constitutional protections to extend to social conditions, technologies, and moral understandings the framers could not have anticipated (electronic surveillance, reproductive technology, evolving conceptions of equal dignity) without requiring the supermajority consensus of Article V amendment for every adaptation.
% TRANSFER_FUNCTION: Moves interpretive authority from elected legislatures and the amendment process to the federal judiciary; moves the practical capacity to settle contested social questions from majoritarian political processes to appointed judges applying contemporary-values reasoning.
% ABSENT_VOICES: Legislative majorities whose enacted preferences are overridden, and citizens who would prefer contested moral and social questions be resolved through the amendment process or ordinary lawmaking rather than judicial doctrine, are not parties to the cases that establish new constitutional readings; their objection surfaces only in political backlash, confirmation battles, and calls for court reform after the fact.
% DISAPPEARANCE_RATIONALE: If living-constitutionalism disappeared as an interpretive method overnight, courts would be confined to text, structure, and historical evidence of original meaning; many currently-recognized rights (certain privacy protections, some equal protection extensions) would require either originalist textual argument or a return to the amendment process, and dozens of precedents built on evolving-standards reasoning would become vulnerable to challenge.
% FOUNDING_PROBLEM: The 1787 text is terse, was drafted for an agrarian 18th-century society, and the Article V amendment process is deliberately difficult (supermajorities in Congress and the states) — living-constitutionalism was built to let constitutional meaning track social, technological, and moral change without requiring that difficult supermajority process for every adaptation.
% FOUNDING_PROBLEM_CORROBORATION: Living-reading proponents (rights claimants, much of the legal academy) attest the founding problem is permanently live because society changes faster than Article V can accommodate. Originalist judges, several sitting justices in dissent, and political scientists studying judicial power attest from outside the beneficiary set that the doctrine has drifted from adaptation-to-necessity into judicial policymaking that substitutes for, rather than supplements, the amendment process the framers deliberately made difficult.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).
:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 at the doctrine's early 20th-century emergence (Lochner-era substantive due process, then the incorporation doctrine) to 0.42 by 2024, tracking the doctrine's expansion from narrow economic-liberty applications to broad social-rights adjudication (privacy, dignity, autonomy claims). Suppression (0.38) reflects that legislative majorities and originalist litigants face a structurally difficult path to reverse a living-reading holding — precedent plus stare decisis plus the practical difficulty of Article V amendment. Theater ratio (0.28) is moderate: real adaptive coordination exists (technology and social change genuinely outpace the amendment process) but a growing share of doctrinal elaboration (tiers of scrutiny, evolving-standards tests) serves to justify outcomes reached on other grounds rather than to constrain them. accessibility_collapse (0.35) and resistance (0.62) are both moderate — alternative interpretive methods (originalism, positivism) remain live and contested, unlike a genuine mountain, and resistance from originalist judges, state governments, and political movements is substantial and organized.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the living reading looks like principled interpretive method adapting an old document to new facts. From the seat of a legislative majority whose statute is struck down under a newly-recognized right, or a state government whose locally-enacted policy is invalidated, the same mechanism looks like extraction of policymaking authority through an unaccountable channel. The engine computes these as different seat-level classifications from the same structural data; neither seat's perception is authoritative on its own.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits as agenda-setter: it administers the doctrine, decides when to invoke evolving standards, and bears none of the doctrine's costs directly (institutional power, arbitrage-grade exit via its own authority over the doctrine's scope). Modern rights claimants and the constitutional law academy are beneficiaries — d skews low, they receive durable protection or professional relevance without needing to win the harder political fight. Legislative majorities, originalist litigants, and state governments are payers — d skews high, their enacted or argued preferences are overridden through a mechanism they do not control and cannot easily reverse (constrained exit — appeal, or multi-decade efforts to reshape judicial composition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a terse 18th-century text plus a deliberately hard amendment process — genuinely persists (contested status: proponents say permanently live, critics say the doctrine has drifted past addressing it). The tangled_rope classification captures that living-constitutionalism is not pure extraction: it solves a real coordination problem (adapting fixed text to changed circumstances) that a strict originalist or strict positivist reading would leave partially unsolved. But it is not a pure Rope either, because the coordination benefit is captured disproportionately by parties who prefer judicial to legislative resolution, and it requires active enforcement (precedent, judicial supremacy over constitutional meaning) against parties who would prefer the amendment process or legislative resolution instead. Classifying this as a mountain (settled interpretive method) or pure rope (costless coordination) would mislabel the asymmetric transfer of authority as neutral adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_vs_capture,
    'Does the living-reading doctrine primarily solve a genuine adaptation problem (text cannot anticipate all future circumstances) or does it primarily function as a mechanism for judicial and elite capture of contested social questions that would otherwise require democratic resolution?',
    'Historical case-by-case analysis of which living-reading holdings tracked broad social consensus already forming through legislative and electoral channels (ratifying an emerging consensus) versus which imposed outcomes well ahead of, or contrary to, contemporaneous majoritarian preference (substituting for democratic process). A consistent pattern of the latter would support the capture reading.',
    'If adaptation-dominant, the tangled_rope classification''s coordination component is well-supported and extraction is the necessary cost of flexibility. If capture-dominant, the coordination story is largely cover and the constraint drifts toward snare from the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_vs_capture, empirical, 'Whether living-constitutionalism tracks social consensus or substitutes for it.').

omega_variable(
    kernel_reading_selection,
    'Is the living reading, the originalist reading, or the positivist reading the more defensible account of what the 1787 kernel actually commits its interpreters to — and is that a question with a determinate answer at all, or is the kernel itself genuinely underdetermined between readings?',
    'No empirical resolution mechanism exists; this is a jurisprudential and philosophical dispute about the nature of legal meaning and constitutional authority, not a factual question with a data-driven answer.',
    'If the kernel is genuinely underdetermined, all three readings are permanently coexisting live options and none can claim to be the ''true'' reading against which the others are measured as deviations. If one reading is objectively more defensible, the others may be better understood as elite-serving departures from the kernel''s actual commitments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether the kernel itself determines a correct reading among the three siblings.').

omega_variable(
    evolving_norms_elite_capture,
    'When courts identify ''evolving standards of decency'' or similarly-framed evolving norms, whose norms are actually being measured — a genuine broad social consensus, or the views of a narrower professional and judicial elite that overlaps heavily with the constitutional law academy that theorizes and legitimates the doctrine?',
    'Comparison of judicially-identified ''evolving norms'' against contemporaneous public opinion polling and state legislative activity at the time of the relevant holdings.',
    'If judicially-identified norms track elite professional opinion more closely than broad public opinion, the coordination story (adapting to genuine social change) is significantly weaker than the extraction story (elite preferences imposed under adaptation framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolving_norms_elite_capture, empirical, 'Whether evolving-standards reasoning tracks genuine social consensus or elite professional opinion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1897, us_constitution_1787__living_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__living_reading, theater_ratio, 1937, 0.14).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_1787__living_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_1787__living_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_1787__living_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__living_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1897, us_constitution_1787__living_reading, base_extractiveness, 1897, 0.18).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__living_reading, base_extractiveness, 1937, 0.28).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_1787__living_reading, base_extractiveness, 1965, 0.36).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_1787__living_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_1787__living_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__living_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1897, us_constitution_1787__living_reading, suppression_requirement, 1897, 0.2).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__living_reading, suppression_requirement, 1937, 0.25).
narrative_ontology:measurement(us_c_su_t1965, us_constitution_1787__living_reading, suppression_requirement, 1965, 0.32).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_1787__living_reading, suppression_requirement, 1990, 0.34).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_1787__living_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__living_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the us_constitution_1787 kernel (living, originalist, positivist), each authored as a separate constraint story with its own ε, beneficiary/victim structure, and classification per the ε-invariance principle. The living reading shows higher extractiveness and suppression than a hypothetical positivist reading would, because it concentrates interpretive discretion in the judiciary rather than distributing it between text and amendment process. Changes in judicial composition or doctrine that shift interpretive practice toward one reading structurally pressure the others by changing which precedents survive and which forms of legal argument gain traction in future cases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
