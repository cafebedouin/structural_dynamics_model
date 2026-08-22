% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Constitutional Authority (Historical Public Understanding)
 *   domain: legal/constitutional/interpretive jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the ORIGINALIST reading of the
 *   contested constitutional authority kernel. The kernel is: what grounds
 *   the legitimate meaning and application of the Constitution? The
 *   originalist reading answers: the meaning fixed at ratification and
 *   recovered through historical public understanding. This reading has
 *   become institutionally dominant in the U.S. judiciary since the 1980s
 *   (rising through the Reagan judicial appointments, stabilized through the
 *   Bush II and Trump appointments, reinforced by Federalist Society
 *   jurisprudential consensus). The constraint carries extraction because the
 *   historical moment originalism privileges (ratification, especially 1789
 *   and 1868) embedded the political voice and worldview of those with power
 *   at that time—primarily propertied men, excluding women, enslaved people,
 *   and non-propertied classes. By locking constitutional meaning into that
 *   historical moment, originalism structurally disadvantages
 *   post-ratification rights claimants and makes constitutional change
 *   require the Article V amendment supermajority rather than judicial
 *   interpretation. The measurement series tracks extractiveness rising from
 *   0.41 (when originalism's institutional dominance was less complete,
 *   ~1980s) to 0.68 (contemporary 2020s dominance). Theater ratio rises from
 *   0.18 to 0.41, indicating growing emphasis on the methodological purity
 *   and neutrality of originalism even as it delivers increasingly
 *   conservative outcomes—a signal of Goodhart drift (the appearance of
 *   objective methodology becoming itself the goal rather than a means to
 *   recoverable historical meaning).
 *
 * KEY AGENTS:
 *   - originalist judiciary: agenda-setter, institutional power, controls which historical sources count as authoritative evidence
 *   - conservative constitutional scholars: institutional beneficiaries, develop and defend originalist jurisprudence, shape law school curriculum and professional norms
 *   - rights-claimants post-ratification: powerless targets with trapped exit, require Article V amendment supermajority to win new rights
 *   - marginalized communities: doubly powerless targets with identity-locked exit, their exclusion from ratification means originalism locks in their marginalization
 *   - progressive reform movements: organized payers, bear higher friction for rights advancement than under alternative interpretive frames
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.68).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.72).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Constitutional Authority (Historical Public Understanding)").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "legal/constitutional/interpretive jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'c6eb2b44-aa9e-4a86-8c5e-98f0809cca02').
narrative_ontology:cs_kernel_codification('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', fixed_text).
narrative_ontology:cs_authority_grounding('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', lineage).
narrative_ontology:cs_interpretation_layer_present('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02').
narrative_ontology:cs_reading_relation('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', foundational, historical_public_understanding_gates_interpretation).
narrative_ontology:cs_axiom_status(historical_public_understanding_gates_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', historical_public_understanding_gates_interpretation, empirically_contingent).
narrative_ontology:cs_axiom('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', secondary, judicial_discretion_constrained_by_historical_evidence).
narrative_ontology:cs_axiom_status(judicial_discretion_constrained_by_historical_evidence, holdable).
narrative_ontology:cs_axiom_grounding('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', judicial_discretion_constrained_by_historical_evidence, instrumental).
narrative_ontology:cs_reference_frame('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', ratification_fixed_meaning).
narrative_ontology:cs_drift_state('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', contemporary_era_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6eb2b44-aa9e-4a86-8c5e-98f0809cca02', '2026-06-19T14:32:18Z').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_constitutional_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, institutional_stability_thesis).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, rights_claimants_post_ratification).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, marginalized_communities).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, progressive_reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution by fixing meaning at ratification and requiring historical public understanding as the interpretive gate. Administers the constraint through judicial opinions, precedent-setting, and oversight of lower courts. Maintains the institutional authority to determine which historical sources are authentic and which interpretive moves are legitimate. Benefits from the constraint by maintaining interpretive authority within a bounded framework and claiming fidelity to law rather than policy.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Develop originalist jurisprudential theory, write academic commentary, testify before legislative and judicial bodies, and shape law school curriculum through Federalist Society networks. Benefit from originalism's institutional dominance by gaining career authority, publication venues, and influence over judicial appointments. Exit options exist (alternative interpretive schools like legal realism, critical legal studies, living constitutionalism) but originalism's ascendancy makes remaining within it professionally rewarding.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_constitutional_scholars, beneficiary,
    institutional, generational, mobile, national).

% Seek constitutional recognition for rights not explicitly enumerated in the 1789 text or with demonstrable historical public meaning at that moment (privacy rights, reproductive autonomy, gender-based equality, economic dignity for laborers, religious conscience accommodation beyond strict historical scope). Under originalist interpretation, their claims are gatekept by historical evidence requirements they cannot satisfy because their rights interests were not salient to the 18th-century public understanding. They bear the cost of judicial closure of their claims without meaningful exit — constitutional amendment requires two-thirds of Congress and three-fourths of state legislatures.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, rights_claimants_post_ratification, payer,
    powerless, biographical, trapped, national).

% Historically excluded groups (women, Black Americans, LGBTQ individuals, religious minorities, non-propertied classes, immigrants) face a structural asymmetry: the historical public understanding at ratification was shaped by their exclusion from the political conversation. Originalism locks in their exclusion by requiring that the understanding of those who held power at ratification be the eternal measure of meaning. Their identity as members of excluded groups makes exit impossible — they cannot un-identify. They cannot use constitutional law to advance remedies because their interests were not part of the historical record that originalism privileges.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, marginalized_communities, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, marginalized_communities, excluded).

% Social movements seeking constitutional advancement of new rights or systemic remedies (labor organizing, civil rights advocacy, environmental protection, gender equality, criminal justice reform) bear the burden of having to prove historical public meaning rather than arguing from moral principles, contemporary values, or changed social conditions. The constraint makes reform require Article V amendment (supermajority consensus) rather than judicial construction, which dramatically slows or blocks reform strategies. Exit options exist (litigation in state courts with different interpretive frames, political organizing for amendment, international human rights advocacy) but are harder and slower than under alternative interpretive frameworks.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, progressive_reform_movements, payer,
    organized, generational, constrained, national).

% Judges committed to evolving constitutional meaning with changing moral understanding and social values are locked out of the dominant federal interpretive framework. Their approach loses institutional legitimacy and authority within the federal judiciary hierarchy. Advancement to appellate positions becomes harder when living constitutionalist jurisprudence is characterized as judicial activism and illegitimate. Exit options exist (retire, move to state courts or academia, write dissents) but all carry professional cost relative to adopting originalism or remaining silent.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_judiciary, excluded,
    institutional, generational, mobile, national).

% The formal amendment procedure specified in Article V of the Constitution (two-thirds of Congress and three-fourths of state legislatures required). This institutional mechanism is the enforcement pathway the originalist constraint points to: rights-claimants must pursue amendment rather than judicial interpretation. The supermajority threshold creates structural bias toward status quo preservation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, article_v_amendment_process, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_text_authority__originalist_reading, article_v_amendment_process).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, non-discretionary mechanism for interpreting the Constitution: fixes meaning at ratification and relies on historical evidence rather than contemporary judicial policy preferences. Coordinates different judicial seats around the same interpretive method, enabling precedent consistency and reducing the institutional appearance of arbitrary judicial will-making. Provides courts with a defensible boundary: interpretation must be grounded in historical evidence, not in judges' policy preferences or moral intuitions.
% TRANSFER_FUNCTION: Moves interpretive authority from post-ratification rights-claimants (who lose the ability to argue from evolved moral understanding and contemporary social values) to originalist interpreters (judges, scholars, and the historical public whose understanding is being recovered). Moves temporal authority: what the dead understood at ratification trumps what the living understand now. Moves power from democratically-emerging constituencies seeking new rights toward defenders of institutional constitutional stability and historical fidelity.
% ABSENT_VOICES: Rights-claimants post-ratification whose interests were not represented in the ratification public sphere (marginalized communities, women, enslaved and recently-enslaved peoples) are structurally absent from the historical record that originalism privileges. They cannot object within originalism's own frame because their objection itself would require the very evolutionary interpretation mechanism the constraint rejects. Living constitutionalist and progressive legal scholars are present in law journals and minority judicial opinions but are largely excluded from the dominant institutional framework of the federal courts.
% DISAPPEARANCE_RATIONALE: If the originalist constraint on constitutional meaning disappeared overnight, judicial power to recognize unenumerated rights, to apply old rights to new circumstances, and to advance constitutional meaning through case law would expand dramatically. The landscape of constitutional rights and remedies would shift significantly: privacy law doctrine could expand, reproductive rights protections could be recognized and strengthened, gender equality jurisprudence could advance, criminal procedure could be modernized, civil rights litigation could be facilitated. The supermajority amendment threshold would lose its institutional justification in interpretive theory (though the political supermajority requirement itself would remain as an Article V fact). Rights-claimants and reform movements would have faster pathways to constitutional victory.
% FOUNDING_PROBLEM: The Framers and ratifiers inscribed their understanding into a fixed constitutional text. Without a stable, historically-grounded method to prevent judges from imposing contemporary policy preferences onto ancient language, the text becomes infinitely malleable and judges become legislators. Judicial restraint and fidelity to law (rather than to judges' policy goals) requires anchoring interpretation to an historical fact: what did the public that ratified this text understand it to mean? Fixed meaning prevents judicial discretion from undermining the rule of law.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars (Randy Barnett, Keith Whittington, Ilya Somin) and originalist judges (Clarence Thomas, Neil Gorsuch, Samuel Alito) attest the problem is live: judicial discretion remains a permanent threat to the rule of law and must be constrained by historical evidence. Living constitutionalist scholars (Laurence Tribe, Erwin Chemerinsky, Jack Balkin) and progressive legal historians (Jill Lepore, David Waldstreicher on Constitutional history) attest the founding problem was adequately solved by the Framers themselves through checks and balances and democratic accountability, and that originalism's proposed solution creates new problems: it locks in historical exclusions, blocks democratic constitutional evolution, and actually enables conservative judicial activism because historical evidence is contestable. Non-partisan sources (Constitutional Law casebooks and law review articles note both readings as live ongoing positions; federal courts split clearly along ideological lines on originalist vs. alternative interpretive methods; public opinion on rights issues diverges from originalist judicial outcomes) confirm the problem's status is genuinely contested, not settled by evidence or theory.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because originalism transfers interpretive power from contemporary rights-claimants to historical publics long deceased, whose understanding was shaped by and served the interests of the powerful at ratification. Suppression is also high (0.72) because the constraint is actively enforced through judicial precedent, law school curriculum emphasis, judicial appointments (the Federalist Society functions as a gatekeeper), and dismissal of alternative interpretive methods as illegitimate. Theater ratio has risen over the 32-unit interval (0.18 to 0.41), indicating that originalism increasingly emphasizes its methodological purity and historical rigor as its justification, while delivering outcomes—conservative shifts on affirmative action, voting rights, gun rights, executive power—that suggest extractive function has become primary. The measurement series captures the constraint's institutional ascendancy and the shift from genuine methodological commitment (early originalism in the 1980s-90s, theater_ratio low) to increasingly performative maintenance of methodological appearance as outcomes become more obviously aligned with conservative policy preferences (contemporary, theater_ratio high). Accessibility collapse is high (0.78) because once originalism is institutionalized as the only legitimate interpretive method, rights-claimants can see no alternative pathway except Article V amendment—the interpretive alternatives (living constitutionalism, moral constructivism) are dismissed as illegitimate within the dominant framework. Resistance remains moderate-high (0.64) because progressive legal scholars continue producing living constitutionalist jurisprudence, rights organizations litigate under alternative frames, and public opinion on many rights issues diverges from originalist outcomes—but this resistance cannot translate into judicial wins within the current institutional configuration.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary (agenda-setter seat, d near 0.0 beneficiary end) experiences the constraint as neutral interpretive method that constrains their own discretion—it provides a boundary that makes their decisions about meaning principled rather than political. Rights-claimants (payer seats, d near 1.0 target end) experience the same constraint as arbitrary gatekeeping that privileges the dead over the living and forecloses remedies they would otherwise win. Marginalized communities (identity-locked payers, d at the extreme target end) experience the constraint as institutional locking-in of their structural exclusion—they cannot exit and cannot use constitutional law to advance their interests because the relevant public understanding at ratification did not include them. The engine should compute markedly different classifications across these seats: originalist judges might compute rope-like (genuine coordination around a stable method), payer seats should compute snare-like (pure extraction with suppression and no real alternatives), identity-locked seats should compute severe snare (identity fusion prevents exit; the constraint became part of their structural marginalization).
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges: d approaches 0.0 (beneficiary). They gain interpretive authority, face internal boundaries but maintain institutional power to define what counts as valid historical evidence, exit is available (they can move to alternative careers) but the constraint rewards remaining within it. Conservative scholars: d near 0.15 (slight beneficiary, mobile exit). They benefit from academic prestige and institutional prominence, but could exit to living constitutionalist or positivist jurisprudence with modest career cost. Rights-claimants: d approaches 1.0 (full target). They lose interpretive pathways, must overcome Article V supermajority to win rights, exit is trapped (they cannot relocate jurisdiction or un-identify as seeking rights). Marginalized communities: d at 1.0+ (exceeds full target). They suffer identity-locked exit (cannot un-identify as marginalized) layered on trapped exit (cannot relocate jurisdiction). The measurement captures how the same constraint's d values diverge sharply across seats: originalist judges receive subsidy-like treatment (their discretion is bounded but their authority is preserved), rights-claimants receive extraction. This divergence should drive per-seat classification divergence: rope-like for agenda-setter, snare for powerless payers, severe snare for identity-locked payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('without fixed meaning, judges will impose policy preferences, and the rule of law dies') was live at the time originalism was theorized (1980s-1990s, when judicial activism on both sides was visible). The problem remains contested: originalists attest it is still live (courts still reach contested results), but rights advocates attest the problem was overblown (the Framers' own constitutional design included checks on judicial power, and social movements can drive amendment when courts disappoint). The originalist answer (fix meaning at ratification) creates a new problem: it locks in historical exclusions and blocks democratic constitutional evolution. The constraint has not resolved its mandated problem; instead, it has become a mechanism for conservative constitutional preservation regardless of whether judicial activism is actually happening. This is not mandatrophy in the strict sense (mandate dissolved, function atrophied) but rather mandate-drift: the original purpose (prevent judicial policy-making) has been partially displaced by a secondary function (block progressive constitutional development). The theater ratio's rise from 0.18 to 0.41 captures this drift: the constraint increasingly performs its methodological purity while delivering conservative policy outcomes, suggesting that prevention of judicial discretion is no longer the primary function—constitutional conservatism is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_ambiguity,
    'What counts as the ''public understanding'' at ratification when the ratification public was radically undemocratic (women, enslaved people, non-property-owners excluded from political participation)? Is originalism really recovering the understanding of the ratifying public, or the understanding of the subset with political voice?',
    'Historical scholarship on ratification practices and who participated vs. who was excluded; comparison of originalist scholarship''s actual citation practices (which sources count; which are dismissed) with the full historical record; examination of whether originalists systematically cite sources from excluded groups and give them equal weight.',
    'If the ratifying public that originalism privileges was itself a constructed subset of the population, then originalism is not recovering a neutral historical fact but encoding the preferences of the powerful at ratification. This would shift the analysis from ''natural law / fixed meaning'' to ''tangled rope with extraction encoded in historical moment selection.'' The constraint would be revealed as a constructed arrangement, not a discovered fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_evidence_ambiguity, empirical, 'Which historical actors'' understanding is actually being recovered by ''original public meaning''?').

omega_variable(
    interpretive_methodology_contestation,
    'When originalists disagree about what the historical evidence shows (as they do on the Second Amendment, the Commerce Clause, the scope of federal power), is that disagreement evidence that historical meaning is determinate (and some originalists are wrong about the evidence), or evidence that the constraint''s core claim (that historical meaning provides a determinate answer) is itself false?',
    'Examination of high-stakes originalist disputes (District of Columbia v. Heller and follow-on cases; Congressional commerce power cases) where originalist methodology yields conflicting conclusions. Check whether originalist scholars can agree on which historical sources are authentic, which are more weighty, and what they prove. If persistent disagreement remains despite shared methodological commitment, the determinate-meaning claim is falsified.',
    'If historical meaning is actually indeterminate despite originalist claims of determinacy, the constraint functions less as a neutral interpretive method and more as a discretionary framework that produces appearance of objectivity while allowing originalists to reach preferred outcomes. This would reclassify the constraint as more purely extractive and suggest higher theater_ratio than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_methodology_contestation, empirical, 'Whether ''original public meaning'' provides determinate answers or provides a discretionary framework that appears determinate.').

omega_variable(
    originalism_internal_subreadings,
    'Within originalism itself, are different originalist judges/scholars holding the same reading, or are there competing originalist readings of what counts as fidelity to historical meaning (original intent vs. original public meaning vs. original law, for example)?',
    'Systematic comparison of originalist judicial opinions and scholarship on a specific contested clause (e.g., the Privileges or Immunities Clause, the Necessary and Proper Clause, the scope of the Commerce Clause). Map which interpretive moves different originalists make and where they diverge.',
    'If originalism contains internal contested subreadings, this constraint story represents ''original public meaning originalism'' specifically, not originalism broadly, and might need decomposition into separate stories per subreading. Alternative: the story remains at the meta-level of ''originalism as the institutional reading that forecloses living constitutionalism,'' with subreadings as secondary contestations within the dominant frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_internal_subreadings, conceptual, 'Whether originalism is a single coherent reading of the kernel or contains multiple competing subreadings.').

omega_variable(
    suppression_structural_or_internalized,
    'Does the high suppression metric (0.72) represent external institutional barriers (rights-claimants must get Article V amendment or lose; living constitutionalists are excluded from the dominant framework), or have rights-claimants internalized originalist constraints (come to believe their post-ratification claims are illegitimate, not just blocked)?',
    'Post-institutional-shift measurement: if the originalist constraint were formally displaced by a living constitutionalist or positivist framework, would suppression of post-ratification rights claims persist or decline? If persistent, the suppression is partly internalized; if it declines, the suppression is primarily structural. Track legal arguments, law review scholarship, and litigation strategies before and after a hypothetical institutional shift to detect whether internalization has occurred.',
    'If suppression is internalized, the constraint''s effective extraction is higher than the structural barriers alone suggest — the targets have incorporated the constraint into their identity and self-understanding. This would increase effective χ for powerless and identity-locked agents and could shift classification from tangled_rope to snare at those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Whether suppression of post-ratification rights claims is structurally imposed or internalized by targets.').

omega_variable(
    originalism_false_summit_candidate,
    'Is originalism a neutral, natural interpretive method (fixed meaning is an objective fact about texts), or does originalism''s rise to institutional dominance benefit identifiable actors who benefit from constitutional conservatism and rigid meaning, making it a constructed constraint that appears natural?',
    'Track the distribution of wins and losses across constitutional axes under originalism: does originalism systematically produce outcomes that advantage powerful institutional actors (executive power, property rights, corporate personhood, limits on civil rights litigation) over distributed rights-claimants? If the pattern is systematic and non-random, that is evidence for false-summit status. Compare with counterfactual: what outcomes would living constitutionalism or positivism produce on the same cases? If outcomes diverge systematically in ways that serve extractive interests, that is false-summit evidence.',
    'If originalism is a false summit (appears natural but is constructed to benefit specific parties), it should be reclassified via the FSM chain and the narrative revised to highlight the beneficiaries (conservative legal academy, Republican-appointed judges, institutional stability gatekeepers) as extraction beneficiaries rather than neutral interpreters. The beneficiary list in base_properties should be expanded to explicitly name the winners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_false_summit_candidate, empirical, 'Whether originalism is a natural interpretive method or a constructed arrangement benefiting specific actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t8, constitutional_text_authority__originalist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__originalist_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__originalist_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__originalist_reading, theater_ratio, 32, 0.41).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement(cons_be_t8, constitutional_text_authority__originalist_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__originalist_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__originalist_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__originalist_reading, base_extractiveness, 32, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.51).
narrative_ontology:measurement(cons_su_t8, constitutional_text_authority__originalist_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__originalist_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__originalist_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__originalist_reading, suppression_requirement, 32, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the constitutional_text_authority kernel. All three readings share the same fixed text (the U.S. Constitution) but derive different meanings and authority structures from it. The three readings are linked as simultaneous institutional presences held by different seats. Originalist reading forecloses living constitutionalist reading (if meaning is fixed, it cannot evolve). Positivist reading influences both by proposing law's authority derives from procedural legitimacy, not moral content. See the three constraint stories for detailed family relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
