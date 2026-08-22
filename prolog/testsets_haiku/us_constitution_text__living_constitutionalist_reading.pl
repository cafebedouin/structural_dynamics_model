% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitution Adaptive Interpretation Constraint
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The Living Constitution reading instantiates the view that constitutional
 *   meaning evolves with society and that judges must adapt principles to
 *   contemporary circumstances. This reading of the U.S. Constitution emerges
 *   as a response to the problem that a text drafted in 1787 cannot directly
 *   address twentieth- and twenty-first-century social contexts. Judges
 *   adopting this reading (e.g., in decisions recognizing same-sex marriage,
 *   protecting abortion access through a privacy-right reading, expanding due
 *   process) claim authority to reinterpret stable constitutional principles
 *   — liberty, equal protection, due process — in light of evolved social
 *   understanding. The reading operates as a tangled_rope: it coordinates
 *   real judicial capacity to address novel rights claims (coordination
 *   function) while simultaneously extracting interpretive authority from
 *   textual constraint and from democratic amendment procedures (extraction
 *   function). The constraint's persistence requires active enforcement
 *   through institutional maintenance of the reading's precedential authority
 *   and through suppression of originalist interpretations as a competing
 *   judicial doctrine.
 *
 * KEY AGENTS:
 *   - Progressive judicial coalitions: justices and appellate judges who author Living Constitution readings, retaining interpretive authority to adapt constitutional meaning
 *   - Rights claimants in changed contexts: groups (same-sex couples, abortion-access seekers, digital-privacy claimants) who benefit when courts recognize evolved protections
 *   - Originalist interpretive communities: judges, scholars, politicians committed to fixed meaning at ratification; their authority is subordinated when Living Constitution readings become settled precedent
 *   - Constitutional meaning claim (non-agent): the proposition that constitutional meaning is fixed at ratification; it is excluded from operative constitutional dialogue under Living Constitution doctrine
 *   - Legislative bodies: formally bound by judicial reinterpretation; excluded from controlling the pace or direction of constitutional meaning
 *   - Supreme Court institution: benefits from doctrinal flexibility enabling it to settle pressing social questions without explicit amendment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.31).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitution Adaptive Interpretation Constraint").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221').
narrative_ontology:cs_kernel_codification('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', fixed_text).
narrative_ontology:cs_authority_grounding('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', extraction).
narrative_ontology:cs_interpretation_layer_present('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221').
narrative_ontology:cs_reading_relation('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', foundational, constitutional_principles_stable_across_eras).
narrative_ontology:cs_axiom_status(constitutional_principles_stable_across_eras, holdable).
narrative_ontology:cs_axiom_grounding('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', constitutional_principles_stable_across_eras, instrumental).
narrative_ontology:cs_axiom('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', foundational, judicial_adaptation_legitimate_constitutional_function).
narrative_ontology:cs_axiom_status(judicial_adaptation_legitimate_constitutional_function, holdable).
narrative_ontology:cs_axiom_grounding('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', judicial_adaptation_legitimate_constitutional_function, deontological).
narrative_ontology:cs_reference_frame('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', flexible_principles_framework).
narrative_ontology:cs_drift_state('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', contemporary_rights_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b2f4d7c-1411-4d46-a4c2-e6e6bd4f1221', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, progressive_judicial_coalitions).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, claims_to_fixed_constitutional_meaning).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, originalist_interpretive_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, supreme_court_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Supreme Court justices and appellate judges who author Living Constitution readings, interpreting constitutional principles through the lens of contemporary social change and evolved practice. They justify decisions (e.g., Obergefell on same-sex marriage, implicit privacy rights on abortion) by locating legitimacy in the Constitution's adaptable principles rather than fixed historical text. Their interpretive authority depends on the reading's institutional acceptance and on public confidence that courts can legitimately update meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, progressive_judicial_coalitions, agenda_setter,
    institutional, generational, constrained, national).

% Social groups seeking constitutional protection for interests that emerge only after ratification (same-sex couples, women seeking abortion access, digital-privacy claimants). They benefit when courts recognize that constitutional principles (equal protection, due process, liberty) adapt to contemporary life. Their access to rights depends on the Living Constitution reading's institutional grip.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_contexts, beneficiary,
    organized, biographical, mobile, national).

% Judges, scholars, and political actors committed to recovering original public understanding at ratification. They argue the Living Constitution reading dismantles the constraint of fixed text and substitutes judicial will; their interpretive authority and democratic legitimacy claims are subordinated when courts adopt adaptive readings. They bear the cost of fighting rear-guard jurisprudential battles against settled Living Constitution precedent.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, originalist_interpretive_communities, payer,
    organized, generational, constrained, national).

% The normative claim that constitutional meaning is fixed at ratification and that interpreters are bound by original understanding, not by judicial will or social change. This claim suffers diminished institutional standing when Living Constitution readings prevail; it is excluded from the operative constitutional dialogue and is maintained only through dissenting opinions and external scholarly networks.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, claims_to_fixed_constitutional_meaning, payer,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(us_constitution_text__living_constitutionalist_reading, claims_to_fixed_constitutional_meaning).

% Congress and state legislatures are formally bound by whatever the Supreme Court declares the Constitution to mean. Under Living Constitution readings, they cannot lock meaning in place through statutory amendment or even formal constitutional amendment once a reading becomes settled — courts retain interpretive authority to adapt meaning again. They would prefer either fixed meaning (enabling legislative long-term planning) or explicit delegation of adaptation to legislatures; instead they face judicial redefinition of their constitutional constraints.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, legislative_bodies, excluded,
    institutional, generational, constrained, national).

% Constituencies advocating for democratic legitimacy, supermajority rule, and the authority of elected bodies over unelected courts. They argue that Living Constitution readings vest interpretive power in judges, bypassing amendment procedures and grounding constitutional change in judicial fiat rather than democratic deliberation. They are outside the conversation when courts adopt Living Constitution readings to settle contested issues without explicit legislative agreement.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, democratic_theory_traditions, excluded,
    analytical, civilizational, analytical, national).

% The institutional apparatus that adjudicates constitutional meaning and enforces its readings. Under Living Constitution doctrine, the Court retains broad discretion to reinterpret principles in light of social change, which expands its institutional authority relative to legislatures and constrains democratic amendment procedures. The Court benefits from doctrinal flexibility that lets it settle pressing social questions without formal constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, supreme_court_institution, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, supreme_court_institution, beneficiary).

% Law professors, legal historians, and constitutional theorists who analyze whether Living Constitution readings are coherent, whether they track social change accurately, and whether they remain defensible in light of originalist and positivist critiques. They produce the intellectual infrastructure justifying or contesting the reading but do not directly enforce it.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, academic_constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, supreme_court_institution).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the constitutional system to address novel social contexts and evolving rights claims without requiring supermajority amendment: judges interpret stable principles (liberty, equality, due process) by applying them to contemporary circumstances, allowing the single text to govern multiple eras without constant textual revision.
% TRANSFER_FUNCTION: Transfers constitutional legitimacy and interpretive authority from static text-bound interpretation toward living judicial adaptation; moves decisional power over novel rights claims from legislatures and amendment procedures toward courts; empowers particular rights claimants while constraining claims rooted in original meaning.
% ABSENT_VOICES: Originalist judges and scholarly communities that argue interpretation must recover original public understanding; democratic theory schools that insist supermajority procedures should govern constitutional change; state legislatures that would prefer fixed constitutional constraints for planning; populations skeptical that courts can reliably track 'social evolution' without substituting judicial policy preferences for constitutional law.
% DISAPPEARANCE_RATIONALE: If Living Constitution doctrine disappeared and courts reverted to strict originalism, constitutional meaning would freeze at ratification language; rights claims that the Living Constitution now recognizes (same-sex marriage, abortion-access dignity interests, digital privacy) would lose judicial protection and would require explicit amendment or legislative creation; the Court's institutional power to settle contested social issues through novel constitutional interpretation would collapse, forcing amendment procedures to work or leaving constitutional questions legislatively unresolved.
% FOUNDING_PROBLEM: The 1787 constitutional text cannot speak directly to twentieth-century contexts (digital surveillance, healthcare as constitutional right, sexual orientation discrimination) without judges interpreting its principles. Strict originalism at ratification would leave the Constitution mute on social realities its framers could not envision, making it an antiquarian document rather than a governing law.
% FOUNDING_PROBLEM_CORROBORATION: Living Constitution advocates (judges, scholars like Jack Balkin, Cass Sunstein) attest the founding problem is live and urgent. Originalists counter that the problem is overstated — the framers used language general enough to cover unforeseen cases without requiring judges to rewrite meaning. Outside the benefiting and opposing parties, legal historians note the historical record shows the Constitution has operated through interpretive change throughout U.S. history (implied powers, incorporation doctrine, commercial-clause expansion), suggesting the problem existed before Living Constitutionalism was theorized, and legislative/amendment history shows supermajority approval for many interpretive shifts (e.g., ratification of civil-rights statutes implementing equal-protection principles the Court had previously read narrowly). The corroboration is mixed: the problem exists, but its necessity and the Living Constitution reading as its only solution are not corroborated outside the benefiting parties.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.58 over the interval, tracking the institutionalization of Living Constitution doctrine. Early in the period (t=0, Warren Court era), the reading coexists with originalist alternatives and adaptive interpretation is experienced as novel and contested; suppression of originalist positions is low because the institutional outcome remains uncertain. By t=30 (contemporary period), Living Constitution readings dominate constitutional law curricula, judicial precedent, and appellate output, while originalism is confined to dissent and academic critique; extractiveness rises as judges increasingly claim authority to adapt meaning without explicit textual warrant. Theater ratio stays low (0.22 at t=30) because the constraint's coordination function is genuine: courts do solve the real problem of applying fixed text to novel contexts. Suppression requirement rises gradually (0.18 to 0.31) as the constraint requires active institutional enforcement to keep originalist alternatives marginal — the suppression does not reach snare levels because originalist voices remain audible in dissents and scholarship, but it prevents originalism from competing for majority institutional control. The measurements are authored on one shared grid (all three metrics at all six time points) enabling temporal analysis of lifecycle drift.
 *
 * PERSPECTIVAL GAP:
 *   The progressive judicial coalition (agenda-setter seat) experiences this as legitimate, necessary constitutional evolution: judges are adapting stable principles to novel realities, solving a real coordination problem (how does one text govern multiple eras?). The originalist interpretive community (payer seat) experiences it as judicial usurpation: judges are imposing contemporary policy preferences under the guise of principle, and the constraint's persistence depends on suppressing the competing reading rather than persuading through principled argument. Originalists argue the real problem (novel contexts) should be solved through amendment, not judicial reinterpretation. Rights claimants (beneficiary seat) experience it as access to rights: without the living reading, same-sex marriage and abortion access would lack constitutional protection. Originalist or positivist readings would deny them those protections despite strong contemporary consensus. The engine's per-seat computation should capture these divergences: progressive coalitions and rights claimants see coordination with low extraction; originalists see pure extraction and suppression; legislatures see authority loss.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive judicial coalitions hold power (institutional, time_horizon=generational, exit_options=arbitrage — they can exit through dissent or retirement but the institution itself is durable). They benefit from the reading's success in retaining and expanding judicial authority; d approaches 0.0 (beneficiary direction). Rights claimants are organized but less institutionally empowered; they benefit from the reading's recognition of evolved protections but have constrained exit (they cannot declare their own constitutional meanings); d is moderate-low, beneficiary-leaning. Originalist communities are organized and institutionally present but their interpretive authority is subordinated; they bear the cost of fighting jurisprudential battles against settled Living Constitution precedent; d is high, moving toward the target end (1.0). Claims to fixed meaning (non-agent) are excluded from the operative institutional dialogue; they are payers only in the sense that the reading's success means those claims lose standing. Legislatures are excluded from controlling constitutional meaning; they are payers in the sense that Living Constitution adaptation can accomplish what they cannot amend into the text. The directionality spread reflects the constraint's asymmetric structure: judges and beneficiary rights claimants sit near d=0.2-0.3 (beneficiaries); originalists sit near d=0.7-0.8 (targets); legislatures and democratic theorists sit near d=0.8 (targets, lacking control over the constraint's operation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the anachronism of a 1787 text addressing 2000s+ social contexts. Living Constitution doctrine claims to solve this by empowering judges to adapt principles. The measurement series shows extractiveness rising as the reading becomes institutionalized, suggesting that as the solution hardens into doctrine, the original problem (addressing novel contexts through interpretation rather than amendment) remains live, but the constraint's function begins to shift from problem-solving toward authority preservation. By t=30, extractiveness is high (0.58), suggesting judges are using the adaptive capacity to settle contested social issues without full democratic deliberation. The mandatrophy question: has the founding problem remained live, or has the institutional solution persisted beyond its justification? The contested founding_problem_status reflects this: Living Constitution advocates claim the problem is urgent (courts must adapt meaning); originalists and positivists claim the problem can be solved through other mechanisms (amendment, legislation, original interpretation). No mandatrophy is declared here because the founding problem's status remains live and contested — the constraint is still solving its stated problem, but the rising extraction suggests the solution is collecting rents (judicial authority preservation) beyond the problem's requirements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_will_vs_principle_discernment,
    'When Living Constitution judges adapt principles to new contexts, are they discerning principles stable across time and applying them faithfully to new facts, or are they importing contemporary policy preferences and retroactively claiming they track constitutional principle?',
    'Comparative analysis of Living Constitution decisions: do adapted readings cluster around outcomes that align with one ideological faction''s policy preferences, or do they distribute across the ideological spectrum? Do courts offer principled articulations that would commit them to outcomes they dislike (counter-preferential reasoning)? Does the composition of the winning coalition in Living Constitution cases correlate with the appointed justices'' prior political commitments?',
    'If adaptive interpretation systematically tracks judge ideology rather than stable principles, the constraint operates as masked judicial will-enforcement (snare), not principled coordination (tangled rope). The claimed type would be false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_will_vs_principle_discernment, empirical, 'Whether Living Constitution interpretation discerns principles or substitutes judicial preferences.').

omega_variable(
    legitimacy_of_unamended_constitutional_change,
    'Does the Constitution''s Article V amendment procedure retain legitimacy as THE mechanism for constitutional change when courts can accomplish the same outcomes through interpretive adaptation without supermajority consensus?',
    'Analysis of institutional capacity and political outcomes: (1) If amendment becomes an alternative pathway that courts themselves sometimes use (as happened with civil rights statutory reinterpretation in the 1960s–80s), the constraint adapts and loses exclusivity. (2) If amendment becomes ceremonial and courts monopolize change, democratic theory predicts loss of legitimacy and institutional crisis. Track whether legislatures or electorates contest Living Constitution decisions via amendment attempts or defiance; if they do, the constraint''s legitimacy is contested and suppression requirement rises. (3) Survey historical periods where Living Constitution readings explicitly reverse prior settled meaning without new amendment (Obergefell reversing the prior marital-definition baseline) vs. periods where interpretation tracks supermajority consensus (implied powers expansion in the 1930s, which Congress had also begun authorizing).',
    'If Living Constitution adaptation becomes the primary change mechanism and Article V atrophies, the constraint operates as constitutional substitution rather than interpretation (moves from tangled rope toward snare). If the two mechanisms coexist and reinforce each other, the constraint retains hybrid legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unamended_constitutional_change, conceptual, 'Whether Living Constitutionalism replaces amendment as the change mechanism or complements it.').

omega_variable(
    reading_identity_vs_framework_identity,
    'Is the Living Constitutionalist reading itself a commitment, or is it an expedient interpretive tool that constitutional frameworks use when they need adaptive capacity? If a judicial coalition changed and originalists took durable control, would Living Constitutionalism persist as a reading or would it be abandoned?',
    'Historical analysis: has Living Constitutionalism philosophy remained stable across changes in judicial personnel, or does it appear when progressive coalitions need interpretive flexibility and disappear or retrench when conservative coalitions want to lock meaning? Does the reading rest on a foundational axiom about how constitutions operate, or on a practical judgment about what judges need to do to remain legitimate?',
    'If Living Constitutionalism is an axiom-level commitment (constitutions by nature adapt), it is foreclosed from originalism (forecloses relation). If it is a tool that both coalitions would use when advantageous, it coexists with originalism (coexists_with relation). If the underlying framework is shared but the reading is a coalition-specific tactic, the relation is influences (changes which tools are politically available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_vs_framework_identity, conceptual, 'Whether Living Constitutionalism is an axiom or an expedient tool.').

omega_variable(
    suppression_of_original_meaning_claims,
    'Does the Living Constitution reading suppress originalist or original-meaning claims through institutional exclusion (they are heard in dissent but not in majority doctrine, limiting their institutional grip), or through internalized abandonment (interpreters are convinced the original meaning is not accessible or relevant)?',
    'Examine originalist dissents and scholarly output: do originalists articulate a coherent alternative reading and claim it should be followed, or do they concede the interpretive problem and contest only the judicial solution? Track whether law-school curricula and judicial clerkships treat original meaning as a live alternative or as a historical curiosity. Survey whether jurisdictions that adopt Living Constitution doctrine see originalist interpretation disappear from local courts or persist in constrained venues.',
    'If suppression is structural (institutional barriers, disciplinary exclusion), the constraint requires active enforcement and suppression remains high. If suppression is internalized (the interpretive community believes Living Constitution is correct), suppression requirement falls and the constraint becomes self-maintaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_original_meaning_claims, empirical, 'Whether suppression of original-meaning claims is structural or internalized.').

omega_variable(
    constitutional_reading_identity_fusion,
    'Have judicial and scholarly communities so fused their identity with Living Constitution doctrine that rejecting the reading is experienced as a professional or intellectual betrayal, making exit from the reading identity-locked rather than merely constrained?',
    'Qualitative analysis: interview or survey judges and scholars who have switched from Living Constitution to originalist readings (or vice versa); examine how professional communities respond to defectors; track whether adopting a new reading is experienced as learning or as apostasy; observe whether law-school hiring, publication, and advancement patterns reward or punish switching readings.',
    'If the reading is identity-locked for its defenders, suppression of alternatives operates through internalized identity fusion: judges and scholars cannot credibly contest it without experiencing self-concept threat. This raises the effective suppression of the originalist reading without requiring overt institutional coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_reading_identity_fusion, empirical, 'Whether Living Constitutionalism is an identity commitment for its defenders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t6, us_constitution_text__living_constitutionalist_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t6, observed).
narrative_ontology:measurement(us_c_tr_t12, us_constitution_text__living_constitutionalist_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t12, observed).
narrative_ontology:measurement(us_c_tr_t18, us_constitution_text__living_constitutionalist_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement_basis(us_c_tr_t18, observed).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_text__living_constitutionalist_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement_basis(us_c_tr_t24, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__living_constitutionalist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t6, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement_basis(us_c_be_t6, observed).
narrative_ontology:measurement(us_c_be_t12, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(us_c_be_t12, observed).
narrative_ontology:measurement(us_c_be_t18, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement_basis(us_c_be_t18, observed).
narrative_ontology:measurement(us_c_be_t24, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement_basis(us_c_be_t24, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(us_c_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t6, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 6, 0.22).
narrative_ontology:measurement_basis(us_c_su_t6, observed).
narrative_ontology:measurement(us_c_su_t12, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement_basis(us_c_su_t12, observed).
narrative_ontology:measurement(us_c_su_t18, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 18, 0.29).
narrative_ontology:measurement_basis(us_c_su_t18, observed).
narrative_ontology:measurement(us_c_su_t24, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement_basis(us_c_su_t24, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement_basis(us_c_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, constitutional_amendment_supermajority_constraint).

% DUAL FORMULATION NOTE:
% The us_constitution_text kernel decomposes into three reading-constraints, each instantiating the same text but with different ε values and beneficiary/victim structures. The living_constitutionalist_reading has high extractiveness (0.58) because it shifts interpretive authority toward judges and away from amendment procedures; the originalist_reading has lower extractiveness (~0.3) because it constrains judges to historical meaning; the positivist_reading has minimal extractiveness (~0.15) because it treats constitutional validity as procedural fact. Each reading is ε-invariant within itself: the living reading's high extraction tracks its claim that judges should adapt meaning, not that the Constitution is inherently extractive. The three readings coexist in the same judicial system, representing different coalitions' interpretive commitments. This story links upstream to the originalist and positivist readings and downstream to constraints like amendment procedure and legislative authority that the living reading affects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
