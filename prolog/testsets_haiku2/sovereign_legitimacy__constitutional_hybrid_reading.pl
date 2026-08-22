% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Sovereign Legitimacy (Dual-Source Reading)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   Under the constitutional hybrid reading, legitimate authority is
 *   dual-sourced: ceremonial and symbolic authority are inherited through
 *   bloodline and tradition (the monarchy), while political authority is
 *   delegated through democratic processes (elected officials).
 *   Constitutional law mediates the boundary, specifying which powers remain
 *   with the crown and which are exercised by parliament or executive. This
 *   reading is one of three structurally distinct accounts of sovereign
 *   legitimacy within the sovereign_legitimacy kernel. It differs from the
 *   monarchical reading (which denies democratic delegation as legitimate and
 *   treats authority as flowing downward from hereditary sovereign) and the
 *   republican reading (which denies hereditary authority as legitimate and
 *   treats authority as flowing upward from popular consent). The hybrid
 *   reading accepts BOTH sources as legitimate within a codified framework,
 *   treating the boundary dispute itself as settled by constitutional
 *   precedent and interpretation. This compromise reduces the extractiveness
 *   both pure forms would impose, but introduces ambiguity costs and
 *   vulnerability to boundary disputes when the constitution's words do not
 *   clearly settle which actor controls which powers. The ε-invariance
 *   principle applies: this story describes extractiveness for the
 *   dual-sourcing arrangement (what the hybrid reading takes the standing
 *   arrangement to be); the monarchical and republican readings describe
 *   different ε values for their respective pure forms (what those readings
 *   take the standing arrangement to be). These are not one constraint viewed
 *   from different angles; they are structurally distinct constraints
 *   emerging from the same contested kernel.
 *
 * KEY AGENTS:
 *   - Hereditary monarch: retains ceremonial authority and reserved powers; benefits from constitutional protection of the institution; identity-locked to the office
 *   - Elected officials: exercise delegated political authority; benefit from popular legitimacy and from the monarch's constitutionally required ceremonial endorsement; constrained by constitutional supermajority and amendment rules
 *   - Citizen electorate: delegate authority and retain theoretical recall power through elections and amendment; benefit from institutional stability but exposed to boundary disputes
 *   - Absolute monarchists: constrained by constitutional supremacy and the neutering of royal prerogative; seek restoration of hereditary authority; ideologically trapped
 *   - Republican purists: constrained by the persistence of the monarchy and constitutional entrenchment; seek abolition of hereditary authority; ideologically trapped
 *   - Constitutional scholars: interpret and referee the boundary through doctrine and precedent; stabilize the hybrid through theoretical coherence; held as observers, not parties
 *   - Constitutional amendment seekers: bear the cost of supermajority requirements and institutional entrenchment; cannot unilaterally shift the boundary; moderate power but constrained exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.38).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.42).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Sovereign Legitimacy (Dual-Source Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '19b795c5-2a17-40aa-96ed-d8c55a0d00e8').
narrative_ontology:cs_kernel_codification('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', formalized).
narrative_ontology:cs_authority_grounding('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', lineage).
narrative_ontology:cs_interpretation_layer_present('19b795c5-2a17-40aa-96ed-d8c55a0d00e8').
narrative_ontology:cs_reading_relation('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', foundational, dual_legitimacy_sources_reconcilable).
narrative_ontology:cs_axiom_status(dual_legitimacy_sources_reconcilable, holdable).
narrative_ontology:cs_axiom_grounding('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', dual_legitimacy_sources_reconcilable, conventional).
narrative_ontology:cs_axiom('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', foundational, constitutional_mediation_of_authority_boundary).
narrative_ontology:cs_axiom_status(constitutional_mediation_of_authority_boundary, holdable).
narrative_ontology:cs_axiom_grounding('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', constitutional_mediation_of_authority_boundary, instrumental).
narrative_ontology:cs_reference_frame('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', constitutional_equilibrium_of_descent_and_consent).
narrative_ontology:cs_drift_state('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', contemporary_democracy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19b795c5-2a17-40aa-96ed-d8c55a0d00e8', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolute_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_purists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_amendment_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizen_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial authority, symbolic legitimacy, and often substantial income (crown estates, allowances) despite ceding political power to elected officials. The hybrid arrangement preserves the institution and its dignity while neutering its capacity to rule by decree. Cannot exit without renouncing the crown entirely—exit means identity dissolution. Under this reading, the monarch's position is secured by constitutional guarantee rather than force of will.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, agenda_setter).

% Govern through delegated popular authority: they exercise political power, set policy, and command the machinery of state. The hybrid reading grants them full operational control while conferring legitimacy through the monarch's ceremonial endorsement and the constitution's interpretive authority. They depend on constitutional law to arbitrate disputes with the crown; they cannot unilaterally redefine their power without triggering a legitimacy crisis or constitutional amendment.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter).

% Under this reading, they delegate power to elected officials who carry popular sovereignty into governance. They retain theoretical recall power (amendment, elections, withdrawal of consent) but exercise it through formal constitutional channels. They benefit from the institutional stability the hybrid arrangement provides; they also bear the cost of ambiguity when the monarch's symbolic role bleeds into political effect.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizen_electorate, beneficiary,
    organized, generational, constrained, national).

% Believe legitimate authority flows downward from the sovereign by divine right and heredity. Under the hybrid reading, they are constrained: the monarch they revere is neutered, the supremacy of the constitution overrides their doctrine, and they have no legal path to restore absolute rule. They can only resist through cultural transmission or revolution—both suppressed by the constitutional order.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolute_monarchists, payer,
    powerless, civilizational, trapped, national).

% Believe legitimate authority flows upward from the people and that all hereditary claims are illegitimate. Under the hybrid reading, they are also constrained: the monarchy persists, the constitution protects it, and popular sovereignty is mediated through the monarch's ceremonial role. They can advocate for republicanism, but the constitutional framework makes formal transition expensive (amendment supermajority, institutional entrenchment).
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_purists, payer,
    powerless, civilizational, trapped, national).

% Interpret and expound the constitution's meaning, especially the boundary between ceremonial and political authority. They referee disputes, propose readings, and construct the theoretical coherence that makes the hybrid arrangement seem natural rather than arbitrary. Their authority is epistemic (credibility, peer review) rather than coercive; they both legitimize and destabilize the reading through continuous reinterpretation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Seek to modify the hybrid arrangement—either to strengthen the crown (monarchical direction) or abolish it (republican direction). Under the hybrid reading, they are constrained: the supermajority requirements for amendment, the entrenchment of the constitution itself, and the institutional interests defending the status quo all raise the cost of transformation. They are not powerless (they can mount sustained campaigns) but cannot unilaterally alter the boundary.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_amendment_seekers, payer,
    moderate, generational, constrained, national).

% Non-hereditary figures (warlords, ideological leaders, corporate powers) who might claim legitimacy on grounds other than heredity or electoral delegation. The hybrid constitutional reading excludes them by channeling all legitimate authority through the monarch-officials dyad. They have no constitutional path to power and can only challenge the order through extra-constitutional means (coup, revolution, institutional capture).
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, rival_legitimacy_claimants, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competing claims to legitimate authority (descent vs. consent, tradition vs. will) by institutionalizing both: the monarch embodies inherited legitimacy and symbolic continuity; elected officials exercise delegated popular authority and policy power. Constitutional law arbitrates the boundary and prevents either pole from dominating. This solves the problem of how a polity can honor tradition while remaining responsive to change.
% TRANSFER_FUNCTION: Transfers ceremonial prestige and symbolic authority FROM the popular base and the state TO the hereditary monarch (income, status, reserved powers); transfers political authority and operative power FROM the monarch TO elected officials and the state apparatus; reserves ultimate sovereignty to the constitution itself, which mediates between the two. The cost of stability is borne by those seeking pure legitimacy forms (monarchists and republicans both constrained).
% ABSENT_VOICES: Rival legitimacy claimants (warlords, ideological movements, non-hereditary elites) are structurally excluded from the framework; they would argue for legitimacy on grounds outside heredity and electoral delegation but find no constitutional channel. Also absent: deep republicans who view any hereditary element as illegitimate by principle, and radical monarchists who view constitutional constraint as illegitimate usurpation of the sovereign's prerogative.
% DISAPPEARANCE_RATIONALE: If the hybrid constitutional arrangement vanished (the constitution invalidated, the monarch deposed, the boundary erased), authority would reconstitute around one of the pure forms—either restored absolute monarchy, elected republic, or violent oscillation between them. The network of laws, precedents, ceremonial practices, and institutional expectations that stabilize the hybrid would collapse, forcing rapid renegotiation of legitimate source and exercise of power.
% FOUNDING_PROBLEM: Early modern polities faced irreconcilable legitimacy claims: monarchy grounded authority in divine right, bloodline, and tradition; emerging popular movements grounded it in consent and rational representation. Pure solutions led to either tyranny (unchecked monarchy) or instability (perpetual conflict over representation). The hybrid constitutional reading emerged to stabilize BOTH claims within a single institutional framework, preserving hereditary dignity while channeling operative power through democratic delegation.
% FOUNDING_PROBLEM_CORROBORATION: The hereditary monarchy attests the founding problem is still live, citing the ongoing need to balance tradition and legitimacy. Elected officials and constitutional scholars attest the founding problem is substantially addressed by the hybrid reading—authority now derives from both sources in a stable codified arrangement. Republican and monarchist movements attest the founding problem is NOT solved, only deferred: they argue the constitution merely suppresses the true conflict rather than resolving it. International comparative evidence from constitutional monarchies (UK, Spain, Netherlands, Belgium, Norway, Sweden, Japan, Canada) supports the empirical viability of the hybrid form; but theoretical disagreement about whether it represents genuine resolution or unstable compromise persists among political philosophers outside the benefiting parties.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.38 at interval end) because the hybrid reading is fundamentally a compromise: both the monarchy and the democratic system receive constitutional legitimacy, which reduces the extraction that either pure form would impose. The monarchy cannot rule by decree (a major constraint on absolute extractiveness), and the electorate has formal channels (elections, amendment) to withdraw consent. This contrasts sharply with the monarchical reading's high ε (absolute rule extracts heavily from the non-royal population) and the republican reading's moderate ε (democratic rule extracts through tax and mandate but legitimizes extraction through consent). The hybrid's ε sits between and below both—it is the compromise cost. Suppression is moderate (0.42) because the constraint's persistence depends on active constitutional enforcement—courts must interpret the boundary, amendments must be blocked through supermajority requirements, and both monarchist and republican resistance must be contained. Suppression is NOT at the high levels of either pure form because neither side has the capacity to eliminate the other through unilateral action; suppression is levied against attempts to shift the boundary, not against the ordinary operation of dual authority. Theater rises modestly (0.38 to 0.50 over the interval) because as the actual locus of political power concentrates in elected officials, the monarch's ceremonial role becomes increasingly performative: state opening of parliament, royal assent, symbolic privy council attendance become the public face while real power resides elsewhere. If theater_ratio continues rising toward 0.7+, the constraint might transition from tangled_rope (genuine dual-sourcing) toward piton (ceremony masking real concentration of power). Accessibility collapse is moderate (0.61) because the constitutional framework constrains both monarchist and republican exit paths: a monarchist cannot restore absolute rule without constitutional amendment or revolution; a republican cannot abolish the crown without the same. The boundary itself is accessible to reinterpretation through constitutional scholarship and judicial review, but not accessible to unilateral redefinition by any single actor. Resistance is moderately high (0.71) because both monarchists and republicans actively contest the reading, even though they are suppressed by the constitutional framework. This is not passive acceptance; it is active resistance held in check by institutional barriers.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (absolute monarchists, republicans, amendment seekers) and the beneficiary seats (the hereditary monarch, elected officials) should compute different constraint types under this reading. From the beneficiary seats, the constraint should compute as tangled_rope: there is genuine coordination (both legitimacy sources are institutionalized) AND asymmetric extraction (the monarchy retains status and income, elected officials retain policy power, while the constrained parties lose the chance to impose their preferred pure form). From the monarchist payer seat, the constraint should compute as snare: they are trapped and constrained, their preferred form is explicitly foreclosed, and suppression is high relative to their ability to resist. From the republican payer seat, also snare: same structure. From the amendment-seeker seat, possibly scaffold (if they believe the hybrid is temporary and eventually transition to a pure form) or tangled_rope (if they see it as a stable extraction mechanism masquerading as coordination). The constitutional scholar's observer seat should compute the constraint as rope or mountain: a stable equilibrium principle of constitutional jurisprudence, not an extraction mechanism. This per-seat divergence is the measurement the framework is designed to take. The claim (tangled_rope) and the metrics are authored independently of any predicted engine output; the engine computes what each seat perceives.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch has d near the beneficiary end (estimated 0.15–0.25): the constitution guarantees institutional preservation, income, and ceremonial prestige; exit is identity-locked (cannot exit without renouncing the crown); power is institutional; time horizon is civilizational. The identity-lock amplifies the beneficiary position—the person is constituted through the office and cannot simply choose to leave. Elected officials have d also in the beneficiary range but slightly higher (estimated 0.25–0.35): they exercise real political power and derive legitimacy from popular delegation, but depend on the constitutional framework to validate their authority; exit is constrained but achievable (electoral loss, political retirement); power is institutional; time horizon is biographical. The shorter time horizon and achievable exit make their directionality slightly more vulnerable than the monarch's. Absolute monarchists and republican purists have d near the target end (estimated 0.80–0.90): their preferred legitimacy form is explicitly suppressed by the constitutional arrangement; they must resist through cultural transmission or extra-constitutional means; exit is trapped (cannot leave the polity without abandoning their entire worldview and identity); power is powerless; time horizon is civilizational. The trapped exit option and ideological identity-lock make this a high-d position. Constitutional amendment seekers have d in the moderate-to-high range (estimated 0.50–0.65): they have moderate power and constrained (but not trapped) exit, but they bear the direct cost of supermajority requirements and institutional entrenchment. The citizen electorate has d near symmetric (estimated 0.45–0.55): they benefit from institutional stability and representational channels, but bear the cost of constitutional constraint on unilateral action and ambiguity when the boundary proves unclear. No directionality overrides are needed; the derivation from beneficiary/victim + exit options + power should produce the right values from the schema's canonical fallbacks.
 *
 * MANDATROPHY ANALYSIS:
 *   Under the constitutional hybrid reading, the founding problem is CONTESTED by the three readings. The hybrid reading claims the founding problem (how to stabilize competing legitimacy claims) is substantially RESOLVED by institutionalizing both descent and consent within a codified constitutional framework. Monarchists claim the founding problem is SOLVED INCORRECTLY—the attempt to balance descent and consent produces neither true authority (which flows from descent alone) nor stable governance (because consent is inherently unstable and illegitimate when applied to the sovereign's prerogative). Republicans claim the founding problem is NOT SOLVED AT ALL—the survival of hereditary authority proves the constraint is not a resolution but a suppression of the true founding problem (how to ground authority solely in popular sovereignty, without residual hereditary claims). The mandatrophy check (founding_problem_status x disappearance_verdict) produces: contested x world_rearranges = a signal that the founding problem's status is under dispute, but the constraint's institutional arrangement is consequential (if it vanished, the world would reorganize). This is not a mandatrophy flag per se (which would require status=dead + verdict=world_rearranges, or status=contested with theater_ratio>0.6 AND minimal functional coordination). However, the rising theater_ratio (0.38 to 0.50) is a warning: if the monarch's ceremonial role becomes increasingly ornamental (theater continuing to rise past 0.6), and if the founding problem remains contested (neither side accepts the hybrid as genuine resolution), the constraint risks drifting from tangled_rope (genuine dual-sourcing) into piton (ceremony masking real concentration of power in elected officials, with the monarchy's coordination function atrophied). At that transition point, the founding problem would be EFFECTIVELY DEAD (the tension between descent and consent is resolved by one side winning), and the constraint would persist by inertia and theatrical maintenance (the ceremonial apparatus) rather than genuine functional necessity. The corpus should watch this trajectory: constitutional hybrid arrangements with rising theater_ratio and contested founding_problem_status are candidates for mandatrophy drift detection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_clarity_under_stress,
    'When the constitution is silent or ambiguous about the boundary between ceremonial and political authority—which party has interpretive authority to resolve the gap? The monarch? Parliament? The courts? Popular vote?',
    'Constitutional crises that force the issue: disputed royal prerogatives (dissolution of parliament, appointment of ministers, veto powers), court cases testing the boundary, or failed constitutional amendments that reveal gaps in the framework. How these are resolved sets precedent.',
    'If the monarch wins interpretive authority, the boundary drifts toward monarchical reading; if elected bodies win it, the boundary drifts toward republican reading; if courts win it, the boundary is mediated by constitutional jurisprudence but remains unstable until the next challenge. The hybrid reading''s stability depends on a clear procedural answer to the authority question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_clarity_under_stress, empirical, 'Who interprets the constitution when the boundary is unclear.').

omega_variable(
    functional_necessity_of_ceremony,
    'Is the monarch''s ceremonial legitimacy function genuinely necessary for the polity''s stability, or is it increasingly ornamental theater that could be eliminated without systemic harm?',
    'Jurisdictional experiments: compare polities that retain ceremonial monarchy with those that transitioned to pure republic (Ireland, France) on measures of civic trust, institutional legitimacy, continuity of governance, and political stability. If outcomes are equivalent or republicans have better outcomes, the ceremony is not functionally necessary.',
    'If ceremony is functionally necessary, the constraint remains tangled_rope (genuine coordination). If ceremony is ornamental, the constraint transitions to piton (theater masking real concentration of power in elected officials). The founding problem shifts from ''how to institutionalize both legitimacy sources'' to ''why do we maintain hereditary ornament if it contributes nothing to governance?''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_of_ceremony, empirical, 'Whether the monarch''s ceremonial role serves a genuine functional purpose or is increasingly performative.').

omega_variable(
    contested_kernel_coexistence,
    'Can the three readings (monarchical, republican, constitutional hybrid) coexist permanently within one institutional framework, or is the hybrid inevitably unstable and will eventually collapse toward one of the pure forms?',
    'Long-term historical observation: the constitutional monarchies have persisted for 100+ years in multiple jurisdictions (UK, Spain, Scandinavia, Netherlands, Belgium, Japan, Canada) without transition to pure form. But this could reflect stability, entrenchment, or lack of sufficient pressure to force the issue. The resolution depends on whether future legitimacy crises force a reckoning or the hybrid persists indefinitely.',
    'If stable coexistence is possible, the hybrid reading is veridical: it represents a genuine equilibrium of legitimacy sources. If eventual collapse is inevitable, the hybrid is provisional: it is temporary suppression of the underlying conflict, which will eventually force resolution. This affects whether the constraint should be classified as rope (stable coordination) or tangled_rope (unstable extraction masquerading as coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_coexistence, conceptual, 'Whether the three legitimacy readings can coexist indefinitely or whether the hybrid is unstable.').

omega_variable(
    alternative_framings_of_legitimacy_boundary,
    'Is the constitutional hybrid reading the only viable framing of the boundary between hereditary and delegated authority, or could the same institutional arrangement be interpreted through different conceptual lenses?',
    'Comparative constitutional jurisprudence: do scholars and courts in different constitutional monarchies interpret the boundary similarly? Or do they produce divergent readings that suggest the boundary is underdetermined by the constitutional text itself?',
    'If the boundary is multiply interpretable, the constitutional hybrid reading is one among several coherent framings—legitimacy is more fragile because the same institutions could be re-read through alternative lenses (e.g., as a monarchical reading with democratic window-dressing, or as a republican reading where the crown is purely ceremonial). If the boundary has a determinate reading, the hybrid is more robust. This affects the accessibility_collapse score: multiply interpretable boundaries are more accessible to challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framings_of_legitimacy_boundary, conceptual, 'Whether the constitutional hybrid reading is uniquely determined by the framework or one among multiple coherent interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sove_tr_t5, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sove_be_t5, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sove_su_t5, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.18).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% Three structurally distinct constraint stories form the sovereign_legitimacy kernel family. The constitutional_hybrid_reading treats legitimacy as dual-sourced (descent + consent, mediated by constitutional law). The monarchical_reading treats legitimacy as flowing downward from hereditary sovereign. The republican_reading treats legitimacy as flowing upward from popular consent. These are not three readings of the same constraint; they are three constraints emerging from one contested kernel. Each story has its own ε (beneficiary/victim structure, extractiveness profile), and the three ε values differ substantially. This constitutional_hybrid_reading story describes ε for the dual-sourcing arrangement. The monarchical and republican stories describe ε for their respective pure forms. The network links them as a family; the omegas document the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
