% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined Composite Transition Ending Elite Dueling (Legal Prohibition + Institutional Substitution + Cultural Shift + Civil War Trauma)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   Between roughly 1830 and 1870 (mapped to interval units 0-40), the
 *   practice of dueling among American elites was dismantled by four
 *   overlapping forces: anti-dueling statutes with oaths and office
 *   disqualifications; institutional substitutes (courts adjudicating
 *   defamation and debt, commercial credit networks assessing reliability); a
 *   dignity-culture shift relocating worth from displayed courage to inner
 *   character; and the Civil War's mass exposure to real killing, which
 *   stripped ritual combat of its romance. This story instantiates the
 *   OVERDETERMINED COMPOSITE READING of the dueling-disappearance kernel: no
 *   single channel was necessary, each was arguably sufficient, and the
 *   transition's cost-benefit structure is the joint product of all four. The
 *   standing arrangement under contest — the referent of epsilon — is this
 *   multi-channel transition complex itself, assessed by the composite
 *   reading's own lights: it coordinated a genuine collective good (ending
 *   elite legal dualism, in which law was optional for powerful men) while
 *   extracting asymmetrically from the honor-culture class whose entire
 *   status-enforcement mechanism was criminalized, ridiculed, and rendered
 *   obsolete. KEY AGENTS (by structural relationship): - state_legislatures:
 *   Agenda-setter, legal channel (institutional/arbitrage) — authored
 *   prohibitions, gains under every mechanism -
 *   state_courts_legal_profession: Agenda-setter and beneficiary
 *   (institutional/constrained) — absorbed jurisdiction, bore prosecution
 *   costs - evangelical_reform_movements: Agenda-setter, cultural channel
 *   (organized/mobile) — supplied denunciation and expulsion machinery -
 *   commercial_credit_elites: Primary beneficiary (powerful/arbitrage) —
 *   replaced the duel as arbiter of standing -
 *   dignity_culture_middle_classes: Beneficiary (organized/mobile) — adopted
 *   new norms at no loss - southern_honor_gentry: Primary target
 *   (organized/identity_locked) — lost their status mechanism, could not
 *   abandon it without ceasing to be themselves - military_officer_class:
 *   Secondary target (organized/constrained) — regimental expectation bound
 *   them to a dying practice - prosecuted_duelists: Target of the legal
 *   channel (moderate/trapped) — fled across state lines to avoid dockets -
 *   historians_of_honor: Analytical observer (analytical/analytical) — sees
 *   all four channels and authors the readings
 *
 * KEY AGENTS:
 *   - state_legislatures: agenda-setter of the legal channel (institutional/arbitrage) — passed statutes whose enforcement varied wildly; positioned to gain whichever mechanism prevailed
 *   - state_courts_legal_profession: agenda-setter with beneficiary secondary role (institutional/constrained) — converted honor disputes into justiciable ones and collected the jurisdiction
 *   - evangelical_reform_movements: agenda-setter of the cultural channel (organized/mobile) — denominational campaigns and communion expulsions priced dueling in salvation
 *   - commercial_credit_elites: primary beneficiary (powerful/arbitrage) — mercantile credit reporting made personal combat irrelevant to assessing a man's word
 *   - dignity_culture_middle_classes: beneficiary (organized/mobile) — the class whose self-conception the new norms flattered; adopted them freely
 *   - southern_honor_gentry: primary target (organized/identity_locked) — planter aristocracy whose rank rested on credible willingness to kill over insult; criminalized and ridiculed, unable to exit without social death
 *   - military_officer_class: secondary target (organized/constrained) — challenge culture persisted longest in the officer corps; courts-martial pressed from above, regimental expectation from beside
 *   - prosecuted_duelists: legal-channel targets (moderate/trapped) — principals and seconds indicted, fined, disqualified; the ones who kept fighting paid in flight and forfeited office
 *   - historians_of_honor: analytical observer (analytical/analytical) — reconstructs the transition from case files and correspondence; the seat from which this reading is authored
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.65).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.55).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined Composite Transition Ending Elite Dueling (Legal Prohibition + Institutional Substitution + Cultural Shift + Civil War Trauma)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '54274a55-9887-473b-b4fd-4a7d4eef7d38').
narrative_ontology:cs_kernel_codification('54274a55-9887-473b-b4fd-4a7d4eef7d38', distributed).
narrative_ontology:cs_authority_grounding('54274a55-9887-473b-b4fd-4a7d4eef7d38', distributed).
narrative_ontology:cs_reading_relation('54274a55-9887-473b-b4fd-4a7d4eef7d38', dueling_disappearance_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('54274a55-9887-473b-b4fd-4a7d4eef7d38', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_axiom('54274a55-9887-473b-b4fd-4a7d4eef7d38', foundational, independent_sufficient_conditions_coexist).
narrative_ontology:cs_axiom_status(independent_sufficient_conditions_coexist, holdable).
narrative_ontology:cs_axiom_grounding('54274a55-9887-473b-b4fd-4a7d4eef7d38', independent_sufficient_conditions_coexist, empirically_contingent).
narrative_ontology:cs_axiom('54274a55-9887-473b-b4fd-4a7d4eef7d38', secondary, monocausal_accounts_are_provisional_abstractions).
narrative_ontology:cs_axiom_status(monocausal_accounts_are_provisional_abstractions, holdable).
narrative_ontology:cs_axiom_grounding('54274a55-9887-473b-b4fd-4a7d4eef7d38', monocausal_accounts_are_provisional_abstractions, instrumental).
narrative_ontology:cs_reference_frame('54274a55-9887-473b-b4fd-4a7d4eef7d38', pluralist_multi_causal_baseline).
narrative_ontology:cs_drift_state('54274a55-9887-473b-b4fd-4a7d4eef7d38', contemporary_historiography, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('54274a55-9887-473b-b4fd-4a7d4eef7d38', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_courts_legal_profession).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_credit_elites).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, dignity_culture_middle_classes).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, evangelical_reform_movements).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_honor_gentry).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, military_officer_class).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, prosecuted_duelists).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, causal_overdetermination_doctrine).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passed anti-dueling statutes, oath requirements, and office-holding disqualifications across the early nineteenth century, often meeting in chambers whose members had dueled or been challenged. Enforcement varied enormously by state and decade — some prosecuted vigorously, others let the statutes sleep for a generation. Because they authored the legal channel while the institutional and cultural channels matured alongside it, they stood to expand their authority whichever mechanism ultimately prevailed.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Converted the disputes gentlemen previously settled by combat — insults, slander, broken words of honor — into justiciable claims, prosecuted duels and their seconds, and absorbed the status-enforcement function into legal doctrine. Gained jurisdiction, fees, and professional standing with every conversion, while bearing the administrative and political cost of prosecutions that local juries frequently refused to sustain.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_courts_legal_profession, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, state_courts_legal_profession, beneficiary).

% Denominational conferences, tract societies, and moral-reform associations that campaigned against dueling as sin against God, expelled duelists from communion, and mocked the practice in religious press. Supplied the cultural channel's organized muscle; gained members, moral authority, and proof of their civilization program from the campaign's success.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, evangelical_reform_movements, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, evangelical_reform_movements, beneficiary).

% Mercantile houses and credit-reporting pioneers who built networks assessing a businessman's reliability from his ledger history rather than his willingness to fight. As wealth shifted from land to liquidity, their reference books replaced the duel as the arbiter of standing. Profited identically under every variant of the transition — law, institutions, culture, or war could take the credit; the ledgers won regardless.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_credit_elites, beneficiary,
    powerful, generational, arbitrage, continental).

% Clerks, professionals, clergy, and shopkeepers whose sense of worth anchored in inner character, self-control, and respectability rather than displayed courage. Staffed anti-dueling societies and reform newspapers, adopted the new norms freely, and surrendered nothing they valued in doing so — the transition's norms were written in their self-image.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, dignity_culture_middle_classes, beneficiary,
    organized, generational, mobile, national).

% Planter-aristocrat class whose rank rested on displayed courage and the credible willingness to kill over an insult. Faced criminalization of their status practices, prosecution, and growing national ridicule, yet abandoning the code meant ceasing to be gentlemen in their own eyes and their neighbors' — the code constituted their identity rather than serving it. Some kept dueling into the 1850s; others moved encounters to the edges of jurisdiction; the class's collective capacity to defend the practice collapsed with the war and their section's political marginalization.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_honor_gentry, payer,
    organized, biographical, identity_locked, regional).

% Officers of the antebellum army and navy, where challenge culture persisted longest and a refusal to answer a challenge could end a career as surely as a bullet. Pressed by courts-martial and dismissal from above, bound by regimental expectation from beside. The war's mass slaughter broke the practice's romance for many survivors, who returned to a corps far less willing to spend its best men on points of etiquette.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, military_officer_class, payer,
    organized, biographical, constrained, national).

% Principals, seconds, and occasionally survivors indicted under anti-dueling statutes — facing fines, imprisonment, or disqualification from office. Many fled across state lines to notorious dueling grounds beyond warrant service, sacrificing estates, careers, and standing to avoid the docket. Individually prominent men, but isolated against the state once charged, with no coalition machinery behind them.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, prosecuted_duelists, payer,
    moderate, immediate, trapped, regional).

% Scholars reconstructing the transition from case files, newspapers, correspondence, probate records, and denominational archives. They weigh the rival causal channels against one another, author the competing readings of the disappearance, and supply the corroborating testimony — from outside every benefiting party — on what the arrangement was for and whether its problem still exists.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historians_of_honor, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__overdetermined_composite_reading, state_courts_legal_profession).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced private lethal self-help among elites with centralized dispute resolution: courts adjudicating insult and defamation, credit networks assessing reliability without combat, political and religious bodies absorbing personal challenges — solving the collective-action problems of feud escalation and of a legal order that the powerful could opt out of.
% TRANSFER_FUNCTION: Moved dispute-resolution authority and status-enforcement capacity from the honor-culture gentry to state judiciaries and commercial intermediaries; moved criminal liability onto gentlemen who retained the practice; transferred the deference economy's currency from personal valor to institutional credential and ledger standing.
% ABSENT_VOICES: Honor-culture practitioners themselves: by the time prohibitions hardened, the gentry's political representation had thinned, and the men whose code was dismantled left testimony chiefly in memoirs, defiance speeches, and private correspondence rather than in the legislatures and reform congresses that decided the matter. Also absent entirely: enslaved people and poor whites, excluded from the honor economy's protections and its proceedings alike, whose stake in ending elite private violence was never solicited by any channel.
% DISAPPEARANCE_RATIONALE: Modern dispute resolution, credit assessment, and political competition all presuppose the transition: defamation litigation, mercantile credit reporting, and electoral norms each occupy ground dueling formerly held. Restore the practice overnight and those arrangements would have to reorganize around ritual combat — or, more precisely, the world that rearranged between 1830 and 1870 is the one that now stands where dueling stood.
% FOUNDING_PROBLEM: Elite private violence made law optional for powerful men: disputes between gentlemen were settled outside courts by ritual combat, publicly demonstrating that legal authority did not bind the honor class, and leaving insult and slander between the powerful with no remedy the weak could invoke or witness.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside every benefiting party: legal-historical scholarship on the decline of dueling in America and Europe attests the founding problem dissolved with the practice itself; surviving anti-dueling statutes are universally treated in that literature as vestigial; denominational archives and postwar veterans' memoirs independently attest that no live constituency remained. No party inside or outside the beneficiary set attests the founding problem as still live.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.65 because the transition complex dismantled an entire class's normative infrastructure across four channels at once — criminalization, jurisdiction-stripping, ridicule, and battlefield disillusionment — while delivering the substitute institutions slowly and unevenly; honor-culture men bore the costs of every channel simultaneously, which is precisely the composite reading's claim. Suppression is 0.55: predominantly structural (statutes, oaths, courts-martial, prosecutorial discretion) with an internalized residue specific to the gentry, who could not exit even where enforcement slept. Theater ratio 0.34 reflects the documented gap between enacted prohibition and enforcement — anti-dueling statutes were honored in breach for decades, and by interval end much of the legal machinery was vestigial performance while the real work was done by banks and newspapers. Accessibility collapse 0.55: exits existed throughout (apology conventions, 'posting' cowardice in print, migration to jurisdictional edges like the famous dueling grounds), so alternatives never fully collapsed — but by interval end no practical path remained for maintaining a public dueling career. Resistance 0.6 encodes decades of open defiance: duels fought under standing prohibitions, juries refusing to convict, Southern persistence into the 1850s. The measurement series run on ONE shared grid (t=0,8,16,24,32,40) so every metric is authored at every examined point; the suppression_requirement series is included because this story specifically tracks enforcement-capacity change — a rise to peak around t=24 (prosecution waves, oath acts, courts-martial discipline) followed by partial decay as normalization made active enforcement unnecessary. Coalition note: the gentry were an organized class with real coalition potential, but their coalition power collapsed with the war and the political marginalization of their section — organization without a future converts to identity-lock, not leverage.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute sharply different types from identical structural data. From the legislature, bench, pulpit, and counting-house, the composite transition is nation-building coordination: each channel solved a piece of the elite-violence problem, and the seats holding those positions experienced the arrangement as the rule of law arriving. From the southern_honor_gentry seat, the same four channels operate as coordinated dispossession — every independent 'cause' of dueling's decline was, from inside the honor economy, another front in the destruction of their normative order. Within the payer set the seats further diverge: the gentry are identity_locked (abandoning the code meant ceasing to be gentlemen in their own eyes — relational and institutional identity fusion, where the honor code constituted gentility itself, so exit equals social death), while prosecuted_duelists are merely trapped (they could and did flee across state lines, paying in estate and office rather than selfhood). The military_officer_class sits between: bound by regimental expectation rather than soul-deep code, constrained but not fused. The observer seat sees all channels at once and prices none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: commercial_credit_elites (arbitrage exit, gains under every mechanism variant) sit nearest the beneficiary pole; dignity_culture_middle_classes (mobile, nothing surrendered) nearly as far; evangelical_reform_movements gain membership and authority but spent real resources campaigning, placing them slightly above pure beneficiary; state_legislatures hold optionality across all mechanisms (arbitrage). state_courts_legal_profession is dual-positioned — agenda_setter collecting jurisdiction and fees, but bearing prosecution costs juries resisted — deriving a d modestly above the pure-beneficiary end. Victim declarations drive the opposite pole: southern_honor_gentry combine victim status with identity_locked exit, pushing them toward the full-target end; prosecuted_duelists are trapped victims of the legal channel specifically; military_officer_class are constrained victims whose extraction is partly self-administered through regimental expectation. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms differentiate every seat the derivation needs to distinguish, and the composite reading's own thesis (channels act jointly, not selectively) argues against hand-tuning individual seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification guards against two symmetrical misreadings. First, the rising theater_ratio and the survival of unenforced anti-dueling statutes invite a piton reading — a dead mandate maintained performatively. But the constraint's substance was the transition itself, which COMPLETED and dissolved into the ordinary legal-commercial order rather than persisting inertially; the vestigial statutes are residue of one channel, tracked honestly by the theater series, not the constraint's body. Second, the gentry's losses invite a snare reading — identifiable victims, coerced compliance. But the coordination function was genuine and net-positive for most participants: ending elite legal dualism (the condition in which law did not bind powerful men) is a paradigmatic collective good, and the honor class was a minority of those governed. The tangled_rope classification holds both truths: real coordination, real asymmetric extraction through the same structure, held together by active enforcement across four channels. On the R5 genealogy: the founding problem (elite private violence making law optional for the powerful) is dead — attested from outside the benefiting parties by legal-historical scholarship and by the simple fact that no constituency remains — while the world nonetheless rearranges around the transition's products. That mismatch is expected signal, not error: it dates the completion of the coordination function and flags the statutory residue for the theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_nonseparability,
    'Can the four causal channels (legal prohibition, institutional substitution, cultural shift, Civil War trauma) ever be empirically separated, or does non-separability function as the composite reading''s shield against falsification?',
    'Comparative-case analysis: European dueling declined without a US-scale civil war; the antebellum South dueled at high rates under prohibitions older than the North''s substitutions; border-state versus deep-South timing gradients isolate channel contributions.',
    'If channels separate cleanly, the composite reading reduces to a weighted sum and per-channel epsilon values become authorable as distinct constraints; if they do not, the composite reading''s epsilon stays irreducibly joint and per-seat classifications inherit the indeterminacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_nonseparability, empirical, 'Whether the composite''s causal pathways are separable by comparative evidence or irreducibly entangled.').

omega_variable(
    victim_set_indeterminacy,
    'Who bears this constraint''s costs — does the victim set depend on which mechanism dominated, and can the composite reading specify victims at all?',
    'Counterfactual weighting: identify which honor-class losses (criminalization, status displacement, prosecution) persist when each channel is hypothetically removed; archive work on who actually was prosecuted versus culturally displaced.',
    'If the legal channel dominated, prosecuted_duelists and southern_honor_gentry are primary victims and effective extraction concentrates on trapped/identity_locked seats; if the cultural channel dominated, victims are diffuse status losers and the constraint moves toward the rope end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_indeterminacy, conceptual, 'Victim-set composition varies with mechanism weighting under the composite account.').

omega_variable(
    kernel_sibling_structure,
    'This constraint is one reading of kernel dueling_disappearance_mechanism — what would the sibling readings (contraction_reading, institutional_displacement_reading) change structurally if instantiated instead?',
    'Author the sibling stories and compare computed classifications: contraction_reading isolates the dignity-culture channel (likely rope-shaped, diffuse victims, low enforcement); institutional_displacement_reading isolates the substitution channel (cleaner beneficiary-victim pairing, lower suppression).',
    'Classification is reading-indexed, not topic-indexed: divergent computed types across siblings measure the historiographical disagreement itself; convergence would suggest the kernel''s contest is rhetorical rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structure, conceptual, 'Committer-frame omega recording kernel membership and sibling deltas.').

omega_variable(
    civil_war_independence,
    'Was Civil War trauma an independently sufficient condition for dueling''s decline, or merely an accelerant of already-operating legal, institutional, and cultural channels?',
    'Timing analysis against the independence criterion: dueling was already marginal in most Northern states before 1861; the war''s distinctive contribution shows up in officer-class abandonment and Southern postwar disillusionment — test whether pre-war trend lines predict post-war extinction without the war term.',
    'If accelerant-only, the composite is a three-channel overdetermination and the military_officer_class seat''s victimhood shifts from constraint-extraction to war-experience; if independent, the four-channel structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_war_independence, empirical, 'Independence of the Civil War channel within the overdetermination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ddm_oc_tr_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ddm_oc_tr_t0, observed).
narrative_ontology:measurement(ddm_oc_tr_t8, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(ddm_oc_tr_t8, observed).
narrative_ontology:measurement(ddm_oc_tr_t16, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(ddm_oc_tr_t16, observed).
narrative_ontology:measurement(ddm_oc_tr_t24, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(ddm_oc_tr_t24, observed).
narrative_ontology:measurement(ddm_oc_tr_t32, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement_basis(ddm_oc_tr_t32, observed).
narrative_ontology:measurement(ddm_oc_tr_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(ddm_oc_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(ddm_oc_be_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(ddm_oc_be_t0, observed).
narrative_ontology:measurement(ddm_oc_be_t8, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(ddm_oc_be_t8, observed).
narrative_ontology:measurement(ddm_oc_be_t16, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(ddm_oc_be_t16, observed).
narrative_ontology:measurement(ddm_oc_be_t24, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(ddm_oc_be_t24, observed).
narrative_ontology:measurement(ddm_oc_be_t32, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(ddm_oc_be_t32, observed).
narrative_ontology:measurement(ddm_oc_be_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(ddm_oc_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(ddm_oc_su_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(ddm_oc_su_t0, observed).
narrative_ontology:measurement(ddm_oc_su_t8, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(ddm_oc_su_t8, observed).
narrative_ontology:measurement(ddm_oc_su_t16, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(ddm_oc_su_t16, observed).
narrative_ontology:measurement(ddm_oc_su_t24, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(ddm_oc_su_t24, observed).
narrative_ontology:measurement(ddm_oc_su_t32, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement_basis(ddm_oc_su_t32, observed).
narrative_ontology:measurement(ddm_oc_su_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(ddm_oc_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why did dueling die?' decomposes into three structurally distinct constraint stories sharing kernel dueling_disappearance_mechanism. contraction_reading isolates the cultural channel (rope-shaped: broad beneficiaries, diffuse victims, minimal enforcement); institutional_displacement_reading isolates the substitution channel (tighter beneficiary-victim pairing: courts and credit networks gain exactly what the honor class loses); this overdetermined_composite_reading prices the JOINT operation of all channels plus Civil War trauma, and therefore carries the widest beneficiary set, the highest enforcement surface, and the least separable epsilon. Epsilon is reading-indexed over a shared referent (the transition complex): the siblings each assess one channel's cost-benefit structure, this reading assesses their simultaneous operation. Each family member links the others via network.affects_constraints; upstream-downstream structure runs from the siblings (component analyses) to this reading (synthesis), with reverse pressure: the composite's current dominance changes the legitimacy conditions under which monocausal sibling accounts can be published and funded.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
