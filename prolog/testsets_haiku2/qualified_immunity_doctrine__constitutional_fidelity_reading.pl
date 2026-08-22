% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional/legal
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional fidelity reading of
 *   qualified immunity doctrine. Under this reading, qualified immunity is a
 *   judicially fabricated doctrine with no constitutional or statutory
 *   authorization. The doctrine is illegitimate not because it produces bad
 *   policy outcomes (though it does), but because it lacks textual,
 *   historical, or structural constitutional grounding. The federal judiciary
 *   created the doctrine in Harlow v. Fitzgerald (1982) to protect official
 *   discretion, and the doctrine's persistence depends on continuous judicial
 *   reinforcement of its core mechanism: the 'clearly established law' test,
 *   which allows judges to retroactively determine that plaintiffs' rights
 *   were not clearly established at the moment of violation. This reading
 *   does not claim qualified immunity is unconstitutional on policy grounds;
 *   rather, it asserts the doctrine itself is an illegitimate exercise of
 *   judicial authority—a constitutional violation by the judiciary disguised
 *   as constitutional protection. The reading treats the entire doctrine as a
 *   snare: the beneficiaries are the judiciary (which gained institutional
 *   power to define the scope of remedies) and law enforcement (which gained
 *   near-absolute civil immunity), while the victims are constitutional
 *   rights-bearers whose remedies are systematically blocked. The
 *   constraint's legitimacy is contested, not resolved by this reading; see
 *   the kernel_context and omega variables for the contested ground.
 *
 * KEY AGENTS:
 *   - Judiciary (institutional seat): Author and enforcer of the doctrine; expands its own power by defining what counts as 'clearly established' law; has sole authority to abolish the doctrine but inverted incentive to maintain it.
 *   - Law enforcement agencies (institutional beneficiary): Gain near-absolute civil immunity and practical impunity for constitutional violations; benefit from doctrine expansion.
 *   - Civil rights plaintiffs (powerless victim): Bear litigation costs with minimal chance of success; trapped in the legal system's framework.
 *   - Constitutional victims (powerless victim, excluded): Experience the constitutional violation in the moment with no remedy; excluded from the legal proceeding that will later declare their rights were not clearly established.
 *   - Congress (excluded institutional actor): Has primary authority to regulate civil liability and police conduct but is excluded from doctrine modification; legislative attempts to restrict immunity are resisted as unconstitutional.
 *   - Comparative legal observers (analytical seat): Document that qualified immunity is idiosyncratic to the U.S. and incompatible with human rights law; have no standing in U.S. courts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.82).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.71).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.39).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional/legal").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'eafccd99-4b37-4fa1-9d92-ec80af90ff20').
narrative_ontology:cs_kernel_codification('eafccd99-4b37-4fa1-9d92-ec80af90ff20', formalized).
narrative_ontology:cs_authority_grounding('eafccd99-4b37-4fa1-9d92-ec80af90ff20', extraction).
narrative_ontology:cs_interpretation_layer_present('eafccd99-4b37-4fa1-9d92-ec80af90ff20').
narrative_ontology:cs_reading_relation('eafccd99-4b37-4fa1-9d92-ec80af90ff20', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('eafccd99-4b37-4fa1-9d92-ec80af90ff20', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('eafccd99-4b37-4fa1-9d92-ec80af90ff20', foundational, judicial_authority_requires_textual_grant).
narrative_ontology:cs_axiom_status(judicial_authority_requires_textual_grant, holdable).
narrative_ontology:cs_axiom_grounding('eafccd99-4b37-4fa1-9d92-ec80af90ff20', judicial_authority_requires_textual_grant, deontological).
narrative_ontology:cs_axiom('eafccd99-4b37-4fa1-9d92-ec80af90ff20', foundational, constitutional_remedies_not_judicially_dispensable).
narrative_ontology:cs_axiom_status(constitutional_remedies_not_judicially_dispensable, holdable).
narrative_ontology:cs_axiom_grounding('eafccd99-4b37-4fa1-9d92-ec80af90ff20', constitutional_remedies_not_judicially_dispensable, deontological).
narrative_ontology:cs_reference_frame('eafccd99-4b37-4fa1-9d92-ec80af90ff20', statutory_remedy_baseline).
narrative_ontology:cs_drift_state('eafccd99-4b37-4fa1-9d92-ec80af90ff20', contemporary_post_civil_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eafccd99-4b37-4fa1-9d92-ec80af90ff20', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary_institutional_power).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The federal judiciary, particularly the Supreme Court, has authored and continuously refined the qualified immunity doctrine since Harlow v. Fitzgerald (1982). The doctrine expands judicial institutional power by concentrating the authority to define what 'clearly established law' means and who gets to enforce constitutional rights. The judges who created and maintain this doctrine enjoy immunity from suit themselves and have incentive to expand rather than constrain the immunity they granted to officers, since narrowing it would invite reciprocal vulnerability. They are the sole institutional seat capable of abolishing the doctrine, yet that capacity is inverted: the more extractive the doctrine becomes, the more the judiciary gains leverage and legitimacy justifying its continued refinement.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary_institutional_power, agenda_setter,
    institutional, generational, analytical, national).

% Police departments, federal agencies, and state law enforcement enjoy a shield against civil liability for constitutional violations so long as they can argue the violated right was not 'clearly established' at the time of the violation. They benefit from the doctrine's practical effect: near-total immunity from suit, combined with immunity from fee liability even when they lose, and the doctrine's built-in delay mechanism (which allows officers to escape dismissal during discovery by arguing the law was not clearly established). Their institutional interests align with doctrine expansion, since broader immunity protects individual officers and reduces accountability costs.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals whose constitutional rights have been violated by law enforcement lack a practical remedy. They must prove their rights were 'clearly established' in existing case law at the moment of violation—a standard that is artificially high because the Supreme Court grants certiorari in very few civil rights cases and because the 'clearly established' test requires factual similarity to prior decisions, not general constitutional principle. Plaintiffs bear the litigation burden and cost with no realistic chance of success in many cases. They cannot exit the constraint: if they are subjected to unconstitutional conduct, the remedy is blocked. They have no alternative legal framework within which to pursue their claims.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Those who experience unconstitutional violations in real time (unlawful arrest, search, deadly force, sexual abuse by officers) are entirely excluded from remedy. They bear the injury—physical, emotional, and financial—without recourse. The constraint exists to deny them standing and voice in the adjudication of their own injury. Unlike civil rights plaintiffs who at least have formal access to courts, victims in the moment have no voice in the legal framework that will later declare their rights were not 'clearly established.'
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_victims, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_victims, excluded).

% Congress has attempted multiple times to legislatively restrict qualified immunity (e.g., George Floyd Justice in Policing Act, multiple iterations of the PEACE Act). The judiciary has resisted legislative override by refusing to acknowledge that Congress can limit a doctrine the judiciary created; the doctrine is presented as constitutionally required, not legislatively granted. Congress is excluded from the decision-making structure despite having primary authority to regulate civil liability and police conduct. Any legislative attempt to restrict the doctrine is met with judicial resistance framed as protecting constitutional separation of powers.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, analytical, national).

% No comparable constitutional democracy employs qualified immunity or anything functionally equivalent. International human rights bodies (UN Human Rights Committee, regional courts) have repeatedly found qualified immunity regimes incompatible with adequate remedy rights. Comparative legal observers can document that the constraint is idiosyncratic to U.S. constitutional interpretation, not a universal requirement of constitutional governance, yet they have no standing in U.S. courts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, comparative_legal_systems, observer,
    analytical, generational, analytical, global).

% An analytical seat representing originalist constitutional interpretation: the doctrine is textually unsupported (no mention of immunity in the Constitution, in § 1983, or in the legislative history of either); originalist logic would demand the doctrine's abolition, yet originalist justices have generally defended and expanded it, revealing tension between stated interpretive methodology and institutional preference for judicial flexibility.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text_originalist_seat, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary_institutional_power).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The doctrine performs no genuine coordination function. It does not solve a collective-action problem involving multiple parties with aligned interests. It solves a unilateral institutional problem: how to insulate judges and law enforcement from accountability while maintaining the appearance of constitutional constraint.
% TRANSFER_FUNCTION: Transfers liability cost from law enforcement agencies and individual officers to civil rights victims and their families. Moves sovereignty over constitutional remediation from the individual rights-bearer and Congress to the federal judiciary. Creates a unidirectional flow of institutional power to the judiciary by giving it exclusive authority to define what law was 'clearly established,' an inherently circular and self-protecting definition.
% ABSENT_VOICES: The constitutional victims themselves are excluded from the legal proceeding that determines whether their rights were violated. Congress, despite its primary authority over civil liability and police regulation, is structurally excluded from doctrine modification. International human rights observers and comparative legal scholars, whose expertise establishes the doctrine's historical and cross-national abnormality, have no standing. Law enforcement unions and civil liberties groups are present in litigation but structurally unable to resolve the underlying dispute because the doctrine is presented as constitutionally mandatory rather than judicially chosen.
% DISAPPEARANCE_RATIONALE: If qualified immunity disappeared overnight, constitutional remedies would return to their statutory baseline: § 1983 liability without judge-created immunities. Victims could sue officers and municipalities. Fee-shifting would attach to losing defendants. Discovery would proceed without artificial dismissal doctrines. The universe of actionable constitutional violations would expand dramatically. Law enforcement practices would shift toward more cautious compliance with the Fourth, Fifth, and Fourteenth Amendments. Judicial workload would increase. The most direct effect would be restoration of individual remedy rights and constitutional accountability.
% FOUNDING_PROBLEM: In Harlow v. Fitzgerald (1982), the Supreme Court asserted that § 1983 (the Civil Rights Act statute authorizing suit against state actors for constitutional violations) needed judicial 'interpretation' to add immunity for government officials, because subjecting officials to suit ex post would chill their decision-making. The Court framed the problem as balancing individual rights against official deterrence. No constitutional text authorized this immunity. No statutory language in § 1983 required it. The Court created it through interpretation.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court has itself acknowledged in later opinions that the 'clearly established' test is too restrictive and that plaintiffs face formidable barriers, yet the Court has refused to abolish the doctrine (see multiple concurring opinions by Justices Thomas, Alito, and others expressing doubts but declining to overrule). Law review scholarship from constitutional scholars outside the judiciary (Schwartz, Bagley, Bahrampour, Reinhardt dissents, and amici representing civil rights organizations) attests that the founding problem—protecting officials from bad-faith litigation—is either solved by summary judgment on qualified grounds, by official immunity doctrine, or by the § 1983 causation and notice requirements, without need for qualified immunity. The empirical record from jurisdictions with narrower immunity (federal law enforcement under some doctrines, state courts that have rejected qualified immunity analogs) shows that official caution increases but not to paralyzing levels. Congress and comparative legal systems attest that constitutional accountability and vigorous law enforcement are not mutually exclusive.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.82): The doctrine blocks constitutional remedies entirely for violations of rights not yet clearly established in case law—a standard deliberately set high because the Supreme Court grants certiorari in very few civil rights cases. For any given constitutional violation, if no prior court has decided an essentially identical factual scenario, the victim's remedy is gone. This is not a harm-reduction measure; it is a mechanism that systematically converts constitutional violations into non-actionable events. Base extractiveness measures how completely the constraint removes remedies, and 0.82 reflects the fact that most violations escape remedy. SUPPRESSION (0.71): The constraint persists through active enforcement (continuous judicial refinement of the 'clearly established' doctrine, expansion of immunity to qualified immunities for various officials, creation of doctrines like 'Iqbal' plausibility pleading that stack on top of immunity). Suppression is high but not maximum because alternative legal frameworks exist (congressional override, state common law, international remedy mechanisms) and because resistance from civil rights organizations, law review scholarship, and legislative coalitions is ongoing and articulate. THEATER (0.48): The doctrine presents itself as balancing individual rights against official discretion, but the actual function is unilateral protection of officials. The stated purpose (preventing excessive litigation from chilling official action) is met by existing doctrines (qualified immunity derived from common law, summary judgment, official immunity). The theatrical element is moderate-high because judicial opinions spend significant effort justifying the doctrine on policy grounds while never addressing its textual invalidity. ACCESSIBILITY_COLLAPSE (0.39): Alternatives are not entirely collapsed. Congress could legislate remedy; state courts could reject immunity analogs; international mechanisms exist; political mobilization for abolition is growing. The constraint depends on suppression, not on alternatives being unavailable. The 'clearly established law' test creates a local collapse (victims cannot see a remedy path), but at the systemic level, alternatives remain visible and contested. RESISTANCE (0.73): High resistance from civil rights organizations, law review scholars, many Democratic legislators, some federal judges, and growing public pressure after high-profile police killings. The constraint persists not because resistance is weak but because the judiciary has sole authority to modify it and inverted incentive to do so. TEMPORAL PATTERN: Extractiveness has risen from 0.68 (1982, shortly after Harlow) to 0.82 (present) because the Supreme Court has progressively refined and expanded the 'clearly established' test, made it harder for victims to survive summary judgment, and extended immunity to new categories of official action. Theater has risen from 0.35 to 0.48 as the gap between stated justification (preventing bad-faith litigation) and actual function (preventing all remedy) has widened, requiring more elaborate judicial theater. Suppression has risen from 0.62 to 0.71 as the enforcement machinery has hardened and coordinated across circuits.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and law enforcement seats experience this constraint as protective and necessary; the civil rights and victim seats experience it as extraction and institutional denial. From the judicial seat, qualified immunity is a reasonable adjustment to § 1983's potentially sweeping liability. From the victim's seat, the doctrine is an absolute bar to remedy. The perspectival gap is not resolvable by better evidence; it reflects a genuine structural conflict: the judiciary benefits from the doctrine, victims are harmed by it. The engine should compute radically different type-classifications across these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary (institutional, analytical exit): d → 0.0–0.1 (strong beneficiary). Benefits from the doctrine by consolidating power to define remedy scope; has analytical exit (can study alternatives without exiting the system). Law enforcement (institutional, arbitrage exit): d → 0.3–0.4 (moderate beneficiary). Benefits from immunity; has arbitrage exit (can move between jurisdictions or agency types). Civil rights plaintiffs (powerless, trapped exit): d → 0.85–0.95 (strong target). Bear litigation costs and costs of failed suits; trapped exit (cannot opt out of the legal system if their rights are violated). Constitutional victims (powerless, trapped exit): d → 0.95–1.0 (absolute target). Injured and excluded from remedy; trapped exit by definition. Congress (institutional, analytical exit): d → 0.5–0.6 (symmetric). Has authority to regulate but excluded from exercise; analytical exit (can study alternatives, propose legislation). The automatic derivation from beneficiary/victim declarations and exit options should produce these directionalities. No overrides needed; the structural data speaks clearly.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE OBSOLESCENCE: The founding problem (preventing chilling of official discretion through ex post liability) is dead. The Supreme Court has acknowledged in recent opinions that the 'clearly established' test is overly restrictive; law review analysis shows that summary judgment, official immunity, and § 1983's causation requirements already address the chilling-effect concern without need for qualified immunity; federal law enforcement operates under narrower immunity regimes and still acts vigorously. The mandate persists because the judiciary has institutional incentive to maintain the doctrine, not because the problem exists. This is the classic Piton signature: the original function has atrophied, but the structure persists through institutional inertia and judicial theater. However, the constraint is classified as SNARE, not Piton, because the extraction is concentrated (judiciary and law enforcement are clear beneficiaries) and the suppression mechanism is active (continuous judicial enforcement). The theater is high enough to indicate performance, but the extraction is real enough to indicate capture rather than mere inertia. The mandatrophy does not reclassify; it flags that the founding problem is dead and the constraint persists as pure institutional power expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authorization_contest,
    'Is the Constitution ''silent'' on official immunity (and therefore permissive), or does the Constitution affirmatively require adequate remedy mechanisms that foreclose judge-created blanket immunity?',
    'Originalist textual analysis of the Fourteenth Amendment''s remedies clause and the Fifth Amendment''s takings-and-due-process language; review of founding-era common law immunity doctrine; systematic comparison to constitutional text that does explicitly authorize immunities (e.g., legislative immunity, judicial immunity, executive privilege).',
    'If the Constitution affirmatively requires adequate remedies, then qualified immunity violates the Constitution; if the Constitution is permissive on immunity, then the judiciary had authority to create it, and the legitimacy question shifts to whether the judiciary exercised authority wisely. The difference is constitutional illegitimacy vs. policy unwisdom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authorization_contest, conceptual, 'Whether the Constitution''s silence on immunity constitutes authorization or prohibition of judge-created immunity.').

omega_variable(
    institutional_incentive_contamination,
    'To what extent does the judiciary''s own immunity from suit for official acts contaminate its judgment about whether law enforcement should have immunity from suit? Does judicial self-interest explain the doctrine''s persistence and expansion?',
    'Comparative analysis: do federal judges who have personal exposure to suit under other doctrines (e.g., Bivens actions) show different patterns of voting on qualified immunity? Do judges from states that limit immunity show different constitutional interpretations when they move to federal courts? Analysis of dissenting opinions: do dissenters cite the judiciary''s own immunity as explaining majority positions?',
    'If institutional self-interest substantially explains doctrine persistence, the legitimacy assessment shifts: the doctrine is not just unauthorized but also the product of conflicted judgment. If self-interest is minimal, the doctrine''s persistence reflects genuine constitutional interpretation, and the illegitimacy assessment narrows to textual authorizedness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_incentive_contamination, empirical, 'Whether judicial immunity from suit contaminates judicial reasoning about officer immunity.').

omega_variable(
    legislative_override_capacity,
    'Does Congress have clear constitutional authority to legislatively abolish qualified immunity, or does the doctrine rest on constitutional ground that Congress cannot override?',
    'Legislative history of § 1983 and its amendments; analysis of whether the Civil Rights Act already contains anti-immunity language that the judiciary has read narrowly; constitutional law scholarship on the boundaries of congressional power to regulate federal jurisdiction and state liability. Test: if Congress passed a statute explicitly abolishing qualified immunity, would the Supreme Court uphold it as within Congress''s authority?',
    'If Congress has clear authority, then judicial resistance to legislative override is institutional power-capture, and the doctrine is illegitimate on separation-of-powers grounds as well as textual grounds. If the doctrine rests on constitutional ground Congress cannot override, then the illegitimacy assessment is narrower (fabrication without authorization) but the remedy is constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_override_capacity, conceptual, 'Whether legislative abolition of qualified immunity is constitutionally possible or foreclosed.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of civil rights remedies structural (external barriers: the doctrine itself, dismissal on summary judgment) or internalized (victims believe remedies are unavailable even when they technically exist)?',
    'Qualitative research: post-dismissal follow-up with civil rights victims—do they understand the doctrine''s blocking effect, or do they perceive remedy-seeking as hopeless for other reasons (cost, time, institutional disbelief)? Measurement: do remedy-seeking rates change if the doctrine is narrowed without publicizing the change?',
    'If suppression is primarily structural (the doctrine itself), then abolishing the doctrine removes the suppression. If suppression is internalized (victims believe remedies are blocked regardless), then doctrine abolition alone is insufficient; victims must also be told remedies exist and are achievable. The constraint''s extractiveness may appear to decrease faster than victim remedies actually increase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of remedies is structural or internalized in victim consciousness.').

omega_variable(
    kernel_alternative_framings,
    'Is the qualified immunity kernel best framed as ''federal statutory protection for law enforcement'' or ''judge-created doctrine without statutory basis''? Does the framing choice pre-judge the legitimacy assessment?',
    'Reread Harlow v. Fitzgerald and subsequent Supreme Court opinions. Does the Court present qualified immunity as interpreting § 1983 or as supplementing § 1983? Do the opinions acknowledge that Congress could have explicitly written qualified immunity into the statute but did not? Does the Court''s framing shift over time?',
    'If the Court frames immunity as interpreting § 1983 (reading implicit authorization), then the legitimacy question is whether the interpretation is texturally sound. If the Court frames immunity as supplementing § 1983 (acknowledging judge-creation), then the legitimacy question is whether the judiciary had authority to supplement. The framing choice affects whether the constitutional fidelity reading is diagnostically strong (doctrine clearly unauthorized) or weaker (interpretation question, reasonable disagreement possible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_alternative_framings, conceptual, 'Whether qualified immunity is framed as statutory interpretation or judicial supplementation, and whether the framing is stable across opinions.').

omega_variable(
    reading_kernel_distinction,
    'Is the constitutional fidelity reading focused on the ACT of judicial doctrine-creation (the institutional illegitimacy), or on the CONTENT of the doctrine created (the immunity itself)?',
    'Careful distinction in commentary and cs_structure between two different claims: (1) The judiciary lacks authority to create immunity doctrines (separation of powers / constitutional fidelity); (2) The immunity doctrine, even if authorized, is substantively illegitimate (policy / rights-based assessment). This reading focuses on (1); the accountability_void reading focuses on (2). They can coexist only if kept distinct.',
    'If this reading is about institutional legitimacy (act of creation), then a Congress-authorized immunity doctrine would be legitimate under this reading, even if the accountability_void reading still opposed it on policy grounds. If this reading is about substantive illegitimacy (content of doctrine), then it collapses into accountability_void. The kernel_context must disambiguate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether constitutional fidelity reading addresses judicial authority to create doctrine or substantive legitimacy of the doctrine created.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(qual_tr_t6, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(qual_tr_t12, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(qual_tr_t18, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 18, 0.44).
narrative_ontology:measurement(qual_tr_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(qual_tr_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(qual_tr_t36, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 36, 0.48).
narrative_ontology:measurement(qual_tr_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 42, 0.48).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(qual_be_t6, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 6, 0.71).
narrative_ontology:measurement(qual_be_t12, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(qual_be_t18, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement(qual_be_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(qual_be_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(qual_be_t36, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 36, 0.82).
narrative_ontology:measurement(qual_be_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 42, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(qual_su_t6, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(qual_su_t12, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(qual_su_t18, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(qual_su_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(qual_su_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(qual_su_t36, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 36, 0.71).
narrative_ontology:measurement(qual_su_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 42, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.25).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, protective_scaffold_reading).

% DUAL FORMULATION NOTE:
% Qualified immunity doctrine has three kernel readings in this analysis: protective_scaffold_reading (immunity is necessary protection), accountability_void_reading (immunity is systematic extraction on policy grounds), and constitutional_fidelity_reading (immunity is institutionally illegitimate because judicially fabricated without authorization). Each story is a separate constraint with its own ε, beneficiary/victim structure, and classification. They share a kernel (the immunity arrangement) but differ in their core premise about legitimacy and function. This reading (constitutional_fidelity) links to the other two via network.affects_constraints. All three readings can coexist in public debate; the network enables contamination analysis to track how undermining one reading's legitimacy affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
