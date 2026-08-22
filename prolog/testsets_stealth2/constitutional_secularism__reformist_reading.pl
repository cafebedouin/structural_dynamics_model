% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Secularism: Affirmative State Duty Against Oppressive Religious Practice
 *   domain: constitutional/political/religious-governance
 *
 * SUMMARY:
 *   Under the reformist reading of constitutional secularism, the state does
 *   not merely permit reform of oppressive religious practice — it owes an
 *   affirmative duty to eliminate such practice, and that duty supersedes
 *   religious-autonomy claims whenever the two collide. The arrangement is
 *   administered principally by constitutional courts applying an
 *   essential-practices style doctrine, backed by legislatures that codify
 *   entry rights, bans, and personal-law reforms, and enforced through police
 *   protection of access and statutory penalties. Its beneficiary set centers
 *   on scheduled castes and women inside religious communities; its payer set
 *   spans hereditary priesthoods, orthodox lay associations, and minority
 *   personal-law bodies across communities. The claim/metric gap is
 *   deliberate and load-bearing: the arrangement is CLAIMED here as
 *   tangled_rope because it pairs a genuine, externally corroborated
 *   protective function with categorical supersession of autonomy and
 *   actively maintained enforcement, while the metrics are authored as
 *   independent descriptive facts — the engine computes per-seat
 *   classifications and any divergence from the claim is the datum. The
 *   epsilon referent is the reformist arrangement itself (the standing
 *   arrangement this story is about), not the strict-neutrality or
 *   principled-intervention alternatives; this file is one member of a
 *   three-story constraint family decomposing the constitutional_secularism
 *   kernel, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - constitutional_reform_courts: agenda-setting interpreter (institutional/analytical) — converts contested custom into enforceable orders and accumulates interpretive supremacy
 *   - social_reform_legislature: co-administrator (institutional/constrained) — codifies the duty into statutes under cross-pressured electoral conditions
 *   - scheduled_castes_and_tribes: primary beneficiary (organized/constrained) — gains enforced access and remedies; cannot exit the caste order, so redress runs through the state
 *   - dalit_temple_entry_activists: frontline beneficiary (organized/constrained) — supplies test cases and absorbs enforcement-point risk
 *   - women_in_religious_communities: dual-positioned beneficiary (moderate/identity_locked) — gains entry and divorce relief while remaining embedded members bearing backlash
 *   - hereditary_priesthoods: primary payer (organized/identity_locked) — loses unilateral gatekeeping over ritual space; office is lifelong identity
 *   - orthodox_practitioner_associations: payer (organized/constrained) — litigates autonomy, absorbs adverse precedent, retains procedural recourse
 *   - minority_personal_law_bodies: payer (organized/constrained) — administers parallel systems under expectation of eventual asymmetric reach
 *   - internal_reform_advocates: excluded voice (moderate/identity_locked) — pursues community-owned reform and objects to state supremacy over interpretation
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — supplies the cross-jurisdictional record both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.68).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.7).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Secularism: Affirmative State Duty Against Oppressive Religious Practice").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional/political/religious-governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '3e850374-776c-4ed7-a9e9-51ee9d18ba3d').
narrative_ontology:cs_kernel_codification('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', fixed_text).
narrative_ontology:cs_authority_grounding('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', extraction).
narrative_ontology:cs_interpretation_layer_present('3e850374-776c-4ed7-a9e9-51ee9d18ba3d').
narrative_ontology:cs_reading_relation('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', constitutional_secularism__principled_intervention_reading, forecloses).
narrative_ontology:cs_reading_relation('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_axiom('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', foundational, anti_oppression_duty_supersedes_religious_autonomy).
narrative_ontology:cs_axiom_status(anti_oppression_duty_supersedes_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', anti_oppression_duty_supersedes_religious_autonomy, deontological).
narrative_ontology:cs_axiom('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', foundational, state_must_dismantle_not_merely_refrain).
narrative_ontology:cs_axiom_status(state_must_dismantle_not_merely_refrain, holdable).
narrative_ontology:cs_axiom_grounding('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', state_must_dismantle_not_merely_refrain, instrumental).
narrative_ontology:cs_reference_frame('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', affirmative_duty_supremacy_framework).
narrative_ontology:cs_drift_state('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', contemporary_selective_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e850374-776c-4ed7-a9e9-51ee9d18ba3d', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes_and_tribes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_in_religious_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, dalit_temple_entry_activists).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, hereditary_priesthoods).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, orthodox_practitioner_associations).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, minority_personal_law_bodies).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, transformative_constitutionalism).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, essential_religious_practices_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hears challenges to exclusionary religious practice, decides which practices count as integral to a faith and which must yield to constitutional guarantees, and issues orders that legislatures and police must carry out. Each ruling extends its interpretive authority over communities that previously governed these questions themselves; the bench also absorbs public criticism whenever its orders meet street-level resistance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_reform_courts, agenda_setter,
    institutional, generational, analytical, national).

% Passes temple-entry statutes, bans on specific practices, and personal-law codifications, and funds the administrative machinery that implements them. Its calendar responds both to reform movements demanding action and to orthodox voting blocs threatening retaliation, so the pace and symmetry of enactment vary with electoral arithmetic.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, social_reform_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Gains legally enforced access to worship spaces, public dignity within religious processions, and statutory remedies against ritual exclusion. Membership in the caste order is not resignable, so redress runs through constitutional organs rather than through leaving the community; reliance on courts and statute is the working substitute for a community-level remedy that never arrived.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes_and_tribes, beneficiary,
    organized, generational, constrained, national).

% Organize entry marches, file the test cases that become precedent, and staff the litigation pipeline. They absorb protest violence and arrest risk at the point of enforcement, and their organizations depend on continued docket success for membership and funding.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, dalit_temple_entry_activists, beneficiary,
    organized, biographical, constrained, regional).

% Receive entry rights, relief from unilateral divorce, and standing to challenge practices conducted in their name. They remain embedded members of the communities whose customs are restructured, so each victory arrives with household and congregational backlash they cannot walk away from without severing kin, faith, and belonging at once.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_in_religious_communities, beneficiary,
    moderate, biographical, identity_locked, national).

% Hold ritual office and the gatekeeping authority that comes with it — controlling who enters, who touches, who officiates. Court orders and statutes remove decisions they previously made unilaterally; the office is a lifelong vocation and family inheritance, so compliance means redefining a role rather than changing jobs.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, hereditary_priesthoods, payer,
    organized, generational, identity_locked, regional).

% Convene lay opinion, fund litigation defending customary practice, and negotiate reinterpretations after adverse rulings. They lose particular practices piece by piece while retaining the associations themselves, and their recourse is procedural delay, doctrinal reframing, and electoral pressure rather than departure.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, orthodox_practitioner_associations, payer,
    organized, biographical, constrained, national).

% Administer parallel family-law systems for their communities and watch the reform power's application to the majority community with the expectation that it will eventually reach them on terms they did not help write. Their defense is litigation and political mobilization; the alternative of pre-emptive internal reform concedes the premise that outsiders may define their tradition.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, minority_personal_law_bodies, payer,
    organized, generational, constrained, national).

% Work for change through theological argument, seminary debate, and community persuasion, and object that transferring interpretive finality to constitutional organs displaces the slower but internally owned process of reform. They are rarely parties to the courtroom coalitions that pair marginalized plaintiffs with reformist benches, and their standing inside their communities makes open opposition to those coalitions costly.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, internal_reform_advocates, excluded,
    moderate, biographical, identity_locked, national).

% Track how different constitutional orders allocate authority over religious practice, publish the comparisons that reformist benches cite and orthodox litigants rebut, and hold no stake in any single jurisdiction's outcome.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, constitutional_reform_courts).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem that neither markets nor ordinary politics solve: people oppressed by their own community's religious practices usually cannot exit the community at acceptable cost, and majority-run political processes will not remedy practices the majority benefits from. The arrangement centralizes the authority to identify and eliminate oppressive practice in constitutional organs, giving isolated internal dissenters a forum they could never win locally.
% TRANSFER_FUNCTION: Moves interpretive and disciplinary authority over religious practice from community and traditional authorities to state constitutional organs; moves access, standing, and status within religious spaces toward marginalized members; moves the enforcement burden onto police, registries, and court dockets.
% ABSENT_VOICES: Internal reform advocates who pursue change through theological self-reinterpretation are structurally outside the courtroom coalitions, as are minority-community representatives who accept that specific practices harm members but reject the premise that state organs may define their tradition. Both would insist on remedies routed through community ownership rather than state supremacy; their absence from the litigation pipeline is what makes the reformist consensus look more unanimous than it is.
% DISAPPEARANCE_RATIONALE: If the affirmative duty vanished overnight, temple-entry rights, personal-law protections, and anti-exclusion enforcement would unravel or collapse back into political contestation that the historical record shows the marginalized losing; traditional authorities would resume gatekeeping wherever enforcement lapsed, and beneficiary movements would lose their principal lever and reorganize around mass mobilization instead.
% FOUNDING_PROBLEM: Religious practices — ritual exclusion of castes, bar on women's entry, unilateral divorce — that immiserate members who cannot exit their communities, entrenched by authorities who benefit from those practices and immune to ordinary majoritarian politics.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: independent caste-atrocity statistics, sociological surveys of actual temple and shrine access, United Nations treaty-body reviews, and the recorded concessions of orthodox institutions themselves, which routinely admit the discriminatory facts while disputing the remedy. No corroborating source attests that the founding problem is dead; the dispute is over the cure, not the disease.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the arrangement's defining move is categorical supersession: autonomy claims do not merely lose particular cases, they lose their status as trumps, and the interpretive authority removed from communities is retained permanently by the bench. Suppression is authored at 0.70 as a raw structural property — it is NOT scaled by power or scope in the engine's arithmetic, unlike extractiveness, which the engine scales by directionality and spatial scope — reflecting enforcement that requires police escorts for entries, statutory penalties, and contempt leverage against resistant institutions. Theater ratio 0.35 captures a real but partial performative layer: declaratory rulings and anniversary commemorations of access that outrun lived access on the ground, commissions that study without compelling. Accessibility collapse sits at 0.48 because alternatives do not vanish — communities reframe doctrine, compensate offices, and litigate boundaries — but the alternative of simply continuing the practice as before is foreclosed once the duty is understood. Resistance 0.60 reflects sustained, organized, multi-decade pushback rather than acquiescence. The temporal series run on one shared grid (t=0,10,20,30,40,50,60,75) with every tracked metric authored at every point, per the alignment rule; all three trajectories rise monotonically. The rising base_extractiveness series is exactly the accumulation signature the T17 abductive trigger watches: extraction layered onto an originally narrower protective mandate as doctrine reaches deeper into communal self-governance. The rising suppression_requirement series models enforcement machinery maturing and hardening — from episodic court orders to standing police-and-statute infrastructure — which is why it is authored despite the general preference for static suppression pictures.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data explains why. From the bench's position the arrangement is duty fulfillment: each ruling extends equal citizenship into a space that withheld it, and the growing docket reads as the constitution working. From the hereditary priesthood's position the same structure operates as dispossession without consent — decisions held for generations are transferred to strangers who then command compliance with the priesthood's own hands. From the women's seat the structure is genuinely double: real gains delivered through a forum that also speaks about them without them, with backlash costs the forum never prices. Same arrangement, three experienced realities; the engine derives this divergence from the declared roles, exits, and locks rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (scheduled castes and tribes, temple-entry activists, women) sit near the full-beneficiary end: the arrangement subsidizes their access and standing, and their constrained or identity-locked exits mean they cannot arbitrage away their dependence on it. Declared payers sit near the full-target end: priesthoods lose gatekeeping rents with identity-locked exit (the office cannot be resigned without self-erasure), while orthodox associations and personal-law bodies bear compliance and precedent costs with only constrained recourse. The agenda-setting organs derive partial directionality — they collect interpretive authority (a real gain) while bearing enforcement and legitimacy costs — so their effective position sits well short of the target end but not at the beneficiary end. Excluded voices are deliberately NOT fed into the derivation: authored absence is commentary-grade evidence about consensus provenance, never a correction-grade input to any seat's classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated, so this is not a mandate outliving its function — the arrangement still does what it was built to do, which blocks any resolved-mandatrophy declaration. The honest worry is drift, not death: theater_ratio climbing from 0.15 to 0.35 tracks declaratory substitution (symbolic rulings replacing enforced access), and the selective-application omega tests whether the duty's reach is tracking practice severity or community identity. On the mismatch consumer: founding_problem_status=live crossed with disappearance_verdict=world_rearranges is the consistent cell — no zombie flag fires. If the selective-application omega resolved toward systematic asymmetry, the expected trajectory is toward a pure-coercion profile wearing a universalist cover story, which is precisely the transition the temporal series would surface first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the constitutional_secularism kernel — the reformist_reading. Would instantiating a sibling reading instead produce a structurally different constraint?',
    'Re-author the arrangement under each sibling: strict_neutrality_reading yields a constraint with no affirmative duty and a near-empty victim set; principled_intervention_reading yields one where autonomy retains defeating weight and imposed costs drop accordingly. Compare the resulting epsilon and type outputs across the three files.',
    'Sibling instantiation changes epsilon materially and plausibly flips the computed type toward rope-side profiles; this file''s classification is valid only for the reformist instantiation and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    selective_application_symmetry,
    'Is the affirmative duty applied symmetrically across religious communities, or does enforcement concentrate on some communities'' practices while others are shielded by autonomy deference?',
    'Comparative docket analysis of intervention rates by community, controlling for the severity of the practice challenged; legislative enactment histories disaggregated by which community''s practice is regulated.',
    'Systematic concentration would place the arrangement''s real costs on specific communities beneath a universalist justification, pushing its effective profile toward pure coercion with a coordination cover story; demonstrated symmetry supports the mixed protective-and-impositional reading claimed here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_application_symmetry, empirical, 'Whether the duty''s reach tracks practice severity or community identity.').

omega_variable(
    gain_capture_by_intermediaries,
    'Do the declared beneficiary seats actually receive the arrangement''s gains, or are they intercepted by state intermediaries, litigation professionals, and political entrepreneurs operating in beneficiaries'' names?',
    'Trace landmark rulings through to lived outcomes — entries actually realized, divorces actually prevented, remedies actually collected — and audit the financing and staffing of the movements that litigate in beneficiaries'' names.',
    'Systematic interception would relocate the receipt surface away from the declared beneficiaries, recasting the persistence logic as custodial rather than liberative and weakening the coordination half of the claimed hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_capture_by_intermediaries, empirical, 'Whether declared beneficiaries or intermediaries capture the arrangement''s gains.').

omega_variable(
    payer_resistance_mechanism,
    'Is traditional-authority resistance driven by material loss of gatekeeping position or by internalized theological conviction about ritual purity and office?',
    'Post-ruling compliance trajectories: authorities who accommodate when material position is compensated reveal material drivers; those who resist identically regardless of compensation reveal conviction drivers.',
    'Conviction-driven resistance raises the true enforcement cost of the duty indefinitely and predicts durable low-level defiance; material-driven resistance is addressable and predicts eventual accommodation — the two imply very different long-run suppression requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_resistance_mechanism, empirical, 'Structural versus internalized sources of payer resistance.').

omega_variable(
    essential_practices_line_drawing,
    'Where the bench draws the line between regulable practice and protected essence determines how much religious life the duty reaches — is that line discovered in the traditions themselves or constructed by the interpreting organ?',
    'Cross-jurisdictional comparison of lines drawn over materially similar traditions, plus internal-consistency analysis of the doctrine''s own successive applications.',
    'If constructed, the duty''s reach tracks the interpreter''s commitments rather than the traditions'' structures, and measured imposition will vary with bench composition rather than with practice severity — destabilizing any single epsilon for the arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(essential_practices_line_drawing, conceptual, 'Framing under-determination in the doctrine that bounds the duty''s reach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__reformist_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__reformist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(cons_tr_t50, constitutional_secularism__reformist_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(cons_tr_t60, constitutional_secularism__reformist_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(cons_tr_t75, constitutional_secularism__reformist_reading, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__reformist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__reformist_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(cons_be_t50, constitutional_secularism__reformist_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(cons_be_t60, constitutional_secularism__reformist_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(cons_be_t75, constitutional_secularism__reformist_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__reformist_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__reformist_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(cons_su_t50, constitutional_secularism__reformist_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(cons_su_t60, constitutional_secularism__reformist_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement(cons_su_t75, constitutional_secularism__reformist_reading, suppression_requirement, 75, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional secularism' conflates three structurally distinct arrangements answering one question — when may the state touch religious practice — with incompatible answers. This file instantiates the reformist_reading (mandated intervention, autonomy categorically superseded; highest imposition on religious autonomy; beneficiaries centered on scheduled castes and women; payers span conservative institutions across communities). The principled_intervention_reading is the doctrinal ancestor: permissive, case-by-case intervention in which autonomy retains independent weight, and its precedents are cited as evidence for the reformist extension — hence the upstream-to-downstream edge. The strict_neutrality_reading is the rival frame (equal distance, no interference) under which most of this arrangement's activity would be impermissible ab initio. Each member carries its own stable epsilon over its own arrangement; the epsilon differences ARE the decomposition, not noise to be reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
