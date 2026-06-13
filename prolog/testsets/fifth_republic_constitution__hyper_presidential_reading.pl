% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Hyper-Presidential Reading: Direct Sovereign Executive
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Fifth Republic Constitution (1958, amended 1962 for direct
 *   presidential election) embodies a contested kernel: the relationship
 *   between a directly elected president and a popularly elected legislature.
 *   This story instantiates the HYPER-PRESIDENTIAL READING: the president as
 *   direct sovereign embodying the national will, constrained minimally by
 *   the legislature. Under this reading, the president holds constitutional
 *   authority to invoke Article 49.3 (government formation and budget passage
 *   without explicit assembly confidence), Article 16 (emergency powers
 *   during grave national crisis), and Article 8 veto power. The legislature
 *   enters the victim set because its formal lawmaking authority is
 *   systematically overridden or bypassed. The presidency (as institution and
 *   the incumbent holder of it) is the beneficiary because the reading
 *   allocates binding sovereign will to the executive office. The theater
 *   ratio rises over time as the emergency justification (Fourth Republic
 *   immobilism, Algerian crisis) recedes and the reading becomes a permanent
 *   governance mode defended by performance rather than necessity. This is
 *   one of three structurally distinct readings of the same constitutional
 *   kernel; the others are the PARLIAMENTARY_CONSTRAINT_READING (president as
 *   coordinated executive requiring legislative authorization for
 *   implementation) and the COHABITATION_EQUILIBRIUM_READING (dual executive
 *   with negotiated authority allocation). The claim/metric gap is
 *   intentional: the constraint is CLAIMED as tangled_rope (coordination of
 *   executive action + legislative check, with asymmetric extraction when
 *   president invokes emergency provisions) while the authored metrics
 *   describe substantially extractive, actively enforced operation with
 *   moderate performative overhead. The divergence is the measurement the
 *   corpus takes.
 *
 * KEY AGENTS:
 *   - incumbent_president: plebiscitary-elected sovereign claiming constitutional right to govern unilaterally via decree, veto, and article 49.3 bypass
 *   - presidency_as_institution: benefits from concentrations of power in the executive office, inherited by successive presidents
 *   - national_assembly: holds formal lawmaking authority (Article 34) but experiences systematic marginalization when president invokes bypass mechanisms
 *   - electorate: provides the president's mandate through direct election but is locked out of interim constraint or veto power
 *   - prime_minister: serves at president's pleasure, implementing presidential will rather than independent executive authority
 *   - constitutional_council: observer seat with limited ability to review emergency decrees or constrain executive prerogatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.68).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.71).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Hyper-Presidential Reading: Direct Sovereign Executive").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, 'c9b9ca13-2ac9-4fd0-beed-387a2973d7b3').
narrative_ontology:cs_kernel_codification('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', fixed_text).
narrative_ontology:cs_authority_grounding('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', extraction).
narrative_ontology:cs_interpretation_layer_present('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3').
narrative_ontology:cs_reading_relation('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', foundational, presidential_plebiscitary_supremacy).
narrative_ontology:cs_axiom_status(presidential_plebiscitary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', presidential_plebiscitary_supremacy, deontological).
narrative_ontology:cs_axiom('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', foundational, direct_election_superior_legitimacy).
narrative_ontology:cs_axiom_status(direct_election_superior_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', direct_election_superior_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', secondary, legislative_constraint_institutional_obstruction).
narrative_ontology:cs_axiom_status(legislative_constraint_institutional_obstruction, holdable).
narrative_ontology:cs_axiom_grounding('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', legislative_constraint_institutional_obstruction, empirically_contingent).
narrative_ontology:cs_reference_frame('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', strong_presidency_plebiscitary_sovereignty).
narrative_ontology:cs_drift_state('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', contemporary_post_founding_emergency, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9b9ca13-2ac9-4fd0-beed-387a2973d7b3', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, senate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, electorate_as_constrained_principals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, electorate_as_constrained_principals).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_political_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, rousseauian_direct_sovereignty).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, plebiscitary_democracy).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, executive_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds executive power directly and invokes constitutional prerogatives (Article 8 executive authority, Article 49.3 government formation bypass, Article 16 emergency powers) to govern with minimal legislative authorization. Justifies actions as embodying the people's direct mandate from the presidential election and claims constitutional right to govern independently of parliamentary obstruction. Holds veto over legislation and can dissolve the National Assembly unilaterally (Article 12). The incumbent's authority derives from plebiscitary election, not legislative delegation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).

% The institutional structure of the presidency benefits from a constitutional reading that concentrates power in the executive office, ensuring that successive presidents inherit broad prerogatives regardless of party or coalition. The institution is strengthened by interpretations that minimize legislative constraint and maximize unilateral executive action capacity. The presidency accumulates rents from discretionary authority over emergency powers, appointments, and policy direction.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, arbitrage, national).

% Holds formal legislative authority under Article 34 but experiences systematic constraint when the president invokes Articles 49.3 and Article 16. The assembly's committees, debate procedures, and amendment powers are overridden by presidential decree or confidence-bypass mechanisms. Deputies bear the cost of marginalization — they cannot initiate major policy, cannot block executive decrees without a supermajority censure, and cannot remove the government except through the narrow 49.2 censure motion. The assembly is locked into a subordinate role by constitutional architecture and presidential interpretation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, national_assembly, excluded).

% The upper chamber has no confidence power over the government and serves an advisory role. It can delay legislation via the Article 45 navette but cannot ultimately veto. When the president uses Article 49.3, the senate is entirely bypassed. Its members represent regional and institutional interests but lack binding constraint on executive action.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, senate, payer,
    moderate, generational, constrained, national).

% Citizens hold the formal power to elect the president directly (since 1962) and are presented as the ultimate source of sovereignty in this reading. However, once elected, the president is insulated from popular constraint except at the next election. Between elections, citizens cannot recall the president, cannot veto decrees, and cannot directly authorize emergency powers — the president claims a mandate that supersedes interim legislative authorization. Citizens benefit from unified executive direction and plebiscitary legitimacy; they also bear the cost of concentrated power and limited interim accountability.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, electorate_as_constrained_principals, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, electorate_as_constrained_principals, payer).

% Out-of-power parties cannot initiate policy, cannot constrain the sitting president's decree power or emergency invocations, and must wait for the next presidential election. Between elections, they hold legislative seats but lack binding power. They pay the cost of systematic exclusion from executive authority and the inability to reverse executive actions through normal legislative channels.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_political_parties, payer,
    moderate, biographical, constrained, national).

% Holds constitutional office and is nominally 'head of government,' but in the hyper-presidential reading, serves at the president's pleasure and implements presidential will rather than independent executive vision. The PM benefits from visibility and administrative authority but pays the cost of political accountability for presidential decisions the PM did not originate. Under this reading, the PM is a subordinate executor, not a co-sovereign.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister, beneficiary).

% Reviews constitutional compliance of laws before promulgation and can strike down provisions. However, it cannot review executive decrees in the Article 16 context and cannot constrain emergency powers once invoked. It serves as an analytical seat with limited binding power to reverse executive action.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Labor unions, business associations, civil rights groups, and advocacy organizations can petition and lobby the assembly and president, but lack institutionalized veto or consent power over executive decrees or emergency invocations. They are heard but structurally excluded from binding authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, civil_society_organizations, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unified, decisive executive direction without parliamentary obstruction: a single decision-maker (the president) authorized by direct popular election to set national policy, allocate resources, and respond to crisis without the delay and compromise of multi-stage legislative authorization. This reading claims it solves the coordination problem of collective action in democracy by concentrating authority in a checked-only-by-elections executive.
% TRANSFER_FUNCTION: Transfers binding policy-setting authority from the legislature (which formally holds Article 34 lawmaking power) to the president (who holds Article 8 executive power and can override legislative preferences via Article 49.3 government formation, Article 16 emergency powers, and veto). Also transfers accountability for policy outcomes from the assembly (which could theoretically deliberate and amend) to the president (who claims direct mandate from voters and governs by decree or government decree without amendment).
% ABSENT_VOICES: Subnational governments and regional assemblies are structurally excluded from this reading's account — the hyper-presidential reading frames the state as a unitary executive-people dyad, leaving territorial power arrangements unvoiced. International courts and supranational constraints (EU law, human rights courts) are also absent from the domestic authority conversation this reading conducts. Labor movements and civil society that reject the plebiscitary legitimacy claim are excluded from the binding authority structure.
% DISAPPEARANCE_RATIONALE: If the constitutional reading authorizing unilateral presidential decree, Article 49.3 bypass, and Article 16 emergency power were eliminated and replaced with a genuinely parliamentary or cohabitation reading, the Fifth Republic's entire operating procedure would reorganize. Policy would require legislative deliberation and amendment. Governments would depend on assembly confidence. Crises would invoke collegial consultation rather than presidential decree.
% FOUNDING_PROBLEM: The Fourth Republic's legislative fragmentation (multiple parties, unstable coalitions, immobilism on colonial policy) required a strong executive empowered to act decisively without parliamentary obstruction. De Gaulle drafted the 1958 Constitution to create a directly elected president with emergency and decree powers to resolve the Algerian crisis and prevent future legislative paralysis.
% FOUNDING_PROBLEM_CORROBORATION: De Gaulle himself and contemporary Gaullist interpreters assert the founding problem (Fourth Republic immobilism) remains perpetually live and justify continuous strong presidentialism as the solution. Parliamentary democracy advocates and opposition scholars attest the founding problem was solved by the 1962 direct election reform and the subsequent normalization of French governance — they argue the hyper-presidential reading is now a rent-extraction mechanism sustained long after its emergency justification. International political scientists and comparative law scholars are divided. No external party has wholly validated one reading; the contest is live among French institutional actors themselves.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.68 at interval end) and rises over the 66-year period because the president's ability to bypass or override legislative preferences becomes increasingly systematized and normalized. Early in the Fifth Republic (1958–1962), extractiveness is lower (0.45) because the direct election mechanism has not yet been established, and emergency invocation is genuinely tied to the Algerian crisis and institutional founding. After 1962, with the direct election secured, extractiveness rises steadily to 0.68 by 2024 because successive presidents regularize the use of Article 49.3 for ordinary legislation (not just government confidence), extend Article 16 interpretations, and govern increasingly by decree without legislative amendment. Theater ratio rises from 0.25 to 0.42 because the emergency justification fades; the constraint persists by constitutional interpretation and institutional practice rather than genuine crisis. By 2024, the hyper-presidential reading defends itself through theatrical emphasis on plebiscitary legitimacy and claims about effective governance rather than through actual emergency conditions. Suppression requirement rises from 0.55 to 0.71 because the constraint must actively exclude and marginalize legislative channels that would otherwise operate; passive non-use of assembly powers would not suffice — the president must use veto, 49.3 bypass, and decree to maintain the reading's operational reality. The assembly must be suppressed — its legitimate authority under Article 34 must be overridden — for the hyper-presidential reading to persist. The suppression is structural (veto power, constitutional prerogatives) but increasingly performative (theatrical claims about mandate and effectiveness rather than genuine crisis necessity). All measurements are on one shared time grid: every metric is authored at every examination point (1958, 1974, 1986, 2000, 2012, 2024), enabling temporal coherence in lifecycle analysis.
 *
 * PERSPECTIVAL GAP:
 *   The president's institutional seat perceives the constraint as coordinating executive action and solving Fourth Republic immobilism (rope-like coordination benefit). The assembly's institutional seat perceives the same constraint as extraction — the transfer of their lawmaking authority to unilateral executive action without their consent or amendment. These are not observer-relative measurements of the same underlying reality; they are structurally asymmetric positions that the engine computes differently. The assembly has no choice about whether the extraction occurs; the president chooses to invoke Articles 49.3 and 16 or to govern with assembly buy-in. That asymmetry is the structure the metrics capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options: The incumbent_president holds institutional power, high arbitrage exit (can leverage presidency into post-office influence), and is declared beneficiary → low d (full beneficiary end, ~0.15). The presidency_as_institution has institutional power, arbitrage exit, and is declared beneficiary → low d (~0.20, slightly higher because it persists across presidents rather than depending on one incumbent's choice). The national_assembly holds organized power but constrained exit (deputies cannot dissolve parliament unilaterally and are locked into 5-year terms) and is declared victim → high d (~0.75). The electorate is powerless, identity-locked exit (citizenship cannot be abandoned without extreme cost), and is declared both beneficiary and payer → symmetric d (~0.50). Opposition parties hold moderate power, constrained exit (must wait for election cycle), and are declared payer → high d (~0.72). The prime minister holds powerful position but identity-locked exit (political career depends on holding office) and is declared payer (despite secondary role as beneficiary in visible governance) → high-symmetric d (~0.68). No directionality overrides are needed because the structural derivation captures the asymmetry: beneficiaries insulated from exit constraints sit low on directionality; victims with constrained exit sit high. The extracted value flows upward to the presidency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic legislative paralysis, Algerian crisis) was LIVE in 1958–1962 and motivated the constitutional emergency executive. By 2024, the problem is DEAD in the specific form that justified the reading: there is no ongoing constitutional crisis requiring unilateral executive decree, no equivalent to the Algerian paralysis, and no imminent collapse of state function that necessitates presidential override of legislative deliberation. Yet the hyper-presidential reading persists and is actively defended by presidential incumbents and Gaullist-tradition jurists. The theater_ratio's rise (0.25 → 0.42) models this drift: the reading is now sustained by performative claims about effective governance and plebiscitary legitimacy rather than by genuine emergency conditions. The suppression_requirement's rise (0.55 → 0.71) models the increasing active force required to maintain the reading: the assembly's legitimate authority under Article 34 must be continuously overridden; absent active presidential override (veto, 49.3 invocation, decree), the assembly would exercise its formal power. This is mandatrophy in the classical sense: the mandate has outlived its function, yet the constraint persists through institutional inertia and the benefiting party's (presidency's) commitment to sustaining it. The reading is not naturally selected; it is maintained by executive choice and constitutional interpretation. A parliamentary reading would not require the same theater and suppression overhead because it would accept legislative constraint as legitimate. The hyper-presidential reading must suppress legislative operation to sustain itself, which is the mark of a constraint that has drifted from coordination toward extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the Fourth Republic''s legislative paralysis genuinely been solved, and if so, does the hyper-presidential reading remain justified by its original emergency warrant?',
    'Historical institutional analysis: (1) track the frequency and severity of legislative gridlock during the Fifth Republic vs. Fourth Republic; (2) document which presidents invoked Article 16 and for what stated emergencies; (3) compare legislative output under hyper-presidential vs. parliamentary governance in peer democracies. If legislative output remains high and crises rare, the original warrant is obsolete.',
    'If the founding problem is solved, the reading drifts from coordination-and-emergency to pure rent extraction. Classification would shift from tangled_rope (coordination + asymmetric extraction) toward snare (extraction riding cover story). The mandatrophy flag would fire, and the constraint would be reclassifiable as a zombie institutional form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the hyper-presidential reading''s founding emergency justification remains live or has become historical.').

omega_variable(
    plebiscitary_legitimacy_vs_deliberative_constraint,
    'Does direct popular election of the president confer a superior democratic legitimacy that justifies minimal legislative constraint, or does it merely distribute authority differently without grading deliberative legitimacy?',
    'Democratic theory and constitutional jurisprudence: (1) compare voter mandate strength (turnout, margin) in presidential vs. assembly elections (most recent data: 2022); (2) document whether referendum/plebiscite results (which claim to measure popular will directly) actually override legislative preferences when invoked; (3) international comparison with other directly elected presidencies: how do they resolve mandate-hierarchy contests? Do popularly elected presidents elsewhere claim the same override authority?',
    'If direct election does NOT confer superior legitimacy (or if the theory is contested), the hyper-presidential reading''s core justification breaks. The constraint would shift toward snare (extraction riding a contested legitimacy claim). If direct election DOES confer superior legitimacy, the reading holds and tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebiscitary_legitimacy_vs_deliberative_constraint, conceptual, 'Whether plebiscitary election legitimizes executive override of legislative deliberation, or whether both branches hold equal democratic legitimacy.').

omega_variable(
    reading_cohabitation_boundary,
    'Does the hyper-presidential reading foreclose the cohabitation reading (both logically incompatible in a single framework), or do the readings coexist as different interpretations of the same constitutional text?',
    'Constitutional doctrine analysis: examine cases where cohabitation occurred (1986–1988, 1993–1995, 1997–2002) and document whether the hyper-presidential reading was explicitly rejected or merely suspended. If the reading reasserted itself immediately after cohabitation ended, they coexist. If cohabitation was treated as a constitutional error or aberration, the hyper-presidential reading forecloses it.',
    'If forecloses: the two readings are logically incompatible; one must be chosen. If coexists_with: they are different operating modes the same constitutional text permits, held by different parties or at different moments. This changes the committer-frame typing for the cohabitation reading (which is a sibling).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_cohabitation_boundary, empirical, 'Whether the hyper-presidential and cohabitation readings are logically foreclosed or structurally coexistent.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the assembly''s marginalization purely structural (external veto and constitutional override by the president) or has it been internalized (deputies have accepted the subordinate role as legitimate, preventing even the formal attempt to exercise Article 34 authority)?',
    'Behavioral evidence: (1) track assembly amendment activity and legislative initiative rates over time; (2) document periods when the assembly asserts its authority despite presidential opposition (rare but documented: 2016, 2023 amendment challenges). (3) Compare with jurisdictions where structural and internalized suppression differ (Italian parliament under Berlusconi; Polish Sejm under PiS) to measure the pattern.',
    'If suppression is internalized, the assembly''s resistance to presidential override is lower than the structural suppression measurement suggests; deputies carry the constraint with them even when given formal opportunity to resist. The effective extraction is then higher than 0.68. If suppression is purely structural, deputies would actively resist if given sufficient opportunity; the constraint''s persistence depends entirely on presidential override mechanisms, not on legislative acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether assembly marginalization is sustained by external constraint or by internalized acceptance of subordinacy.').

omega_variable(
    reading_instantiation_uncertainty,
    'Is this story a reading of the actual Fifth Republic Constitution, or a reading of a *de Gaulle''s interpreted* Fifth Republic that has drifted from the constitutional text itself?',
    'Constitutional linguistics: compare the hyper-presidential reading''s interpretation of Articles 8, 12, 16, 34, 49.3 with the text''s literal scope and with constitutional assembly debates (1958). If the reading extends Article 16 emergency powers beyond the text''s original scope, or if 49.3 is used for ordinary legislation (not government confidence), the reading may be of a practice that has drifted from the kernel text.',
    'If the reading has drifted from the text: the kernel is not the Constitution but rather de Gaulle''s practice and its institutional successors. The story would be reclassified as a PITON (degraded institutional form maintained by successors to its founder) rather than a reading of a contested but coherent kernel. The constraint-type would drop from tangled_rope toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_instantiation_uncertainty, conceptual, 'Whether the hyper-presidential reading is a valid interpretation of the Fifth Republic Constitution or a de Gaulle-era practice that has become institutionalized beyond the text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1958, 0.25).
narrative_ontology:measurement_basis(fift_tr_t1958, observed).
narrative_ontology:measurement(fift_tr_t1974, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1974, 0.28).
narrative_ontology:measurement_basis(fift_tr_t1974, observed).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1986, 0.35).
narrative_ontology:measurement_basis(fift_tr_t1986, observed).
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(fift_tr_t2000, observed).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2012, 0.41).
narrative_ontology:measurement_basis(fift_tr_t2012, observed).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(fift_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1958, 0.45).
narrative_ontology:measurement_basis(fift_be_t1958, observed).
narrative_ontology:measurement(fift_be_t1974, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1974, 0.52).
narrative_ontology:measurement_basis(fift_be_t1974, observed).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1986, 0.61).
narrative_ontology:measurement_basis(fift_be_t1986, observed).
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement_basis(fift_be_t2000, observed).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2012, 0.67).
narrative_ontology:measurement_basis(fift_be_t2012, observed).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(fift_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1958, 0.55).
narrative_ontology:measurement_basis(fift_su_t1958, observed).
narrative_ontology:measurement(fift_su_t1974, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1974, 0.59).
narrative_ontology:measurement_basis(fift_su_t1974, observed).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1986, 0.64).
narrative_ontology:measurement_basis(fift_su_t1986, observed).
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(fift_su_t2000, observed).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement_basis(fift_su_t2012, observed).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(fift_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__hyper_presidential_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Fifth Republic Constitution's allocation of executive authority. All three readings are instantiated as separate constraint stories with distinct ε values and beneficiary/victim structures. The hyper-presidential reading claims ε=0.68 (high extractiveness from legislature); the parliamentary reading claims ε≈0.25 (low extractiveness, genuine coordination); the cohabitation reading claims ε≈0.45 (moderate, with power-sharing overhead). Each reading is structurally distinct because the beneficiary/victim sets differ, the claimed coordination function differs, and the mandate-status of legislative authority differs. They are linked via network.affects_constraints because adoption of one reading influences the interpretive environment for the others, and because disputes between the readings constitute the ongoing political contest over constitutional meaning. ε-invariance requires separate stories: the same constitutional text yields different extracted values depending on which reading is operant, and that variation is what the corpus measures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
