% ============================================================================
% CONSTRAINT STORY: dignity_kernel__imago_dei_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__imago_dei_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dignity_kernel__imago_dei_reading
 *   human_readable: Imago Dei Dignity Constraint: Equal Standing Prior to Capability (Technology Governance Reading)
 *   domain: theological ethics / technology governance / philosophical anthropology
 *
 * SUMMARY:
 *   A doctrine old beyond the interval here measured — dignity as the
 *   inviolable image of the Triune God, conferred equally on every person
 *   prior to any capability — operates in the contemporary world as a
 *   technology-governance constraint. It requires that artificial
 *   intelligence remain a tool subordinate to the human person, categorically
 *   rejects human enhancement and the pursuit of superintelligence as
 *   violations of the created order, and forbids any ranking of persons by
 *   cognitive capacity, productivity, developmental stage, or prognosis. This
 *   story measures the constraint's operation as a governance structure from
 *   the first IVF birth (1978) through the contemporary AI governance debate
 *   (2026): a teaching authority defines and enforces it, religious
 *   healthcare institutions administer it at the bedside, powerless classes
 *   at the margins of capability are protected by it, and identifiable
 *   parties — enhancement seekers, enhancement researchers, the advanced-AI
 *   development community where the doctrine's influence reaches, and
 *   patients refused lawful interventions in governed institutions — bear its
 *   categorical costs. The claimed type is the reading's own: created order,
 *   not human artifact. The metrics are authored from documented operation,
 *   independently of that claim.
 *
 * KEY AGENTS:
 *   - magisterial_teaching_authority: agenda setter and principal collector (institutional / identity_locked) — defines, guards, and enforces the doctrine; its authority is constituted by the constraint's persistence
 *   - cognitively_disabled_persons: primary protected beneficiary (powerless / trapped) — standing secured prior to any capability
 *   - persons_at_margins_of_life: protected beneficiary (powerless / trapped) — the unborn, the demented, the persistently unconscious
 *   - religious_healthcare_institutions: beneficiary and frontline enforcer (institutional / identity_locked) — mission identity constituted by doctrinal refusal
 *   - faith_communities_holding_doctrine: beneficiary constituency (organized / identity_locked) — the shared anthropology is the community's moral language
 *   - persons_in_ai_mediated_care: protected beneficiary bearing diffuse optimization costs (powerless / trapped)
 *   - enhancement_seeking_patients: primary cost-bearer (moderate / constrained) — categorical bar, no permitted version of the request
 *   - human_enhancement_researchers: cost-bearer (organized / constrained) — program purposes barred in principle
 *   - agi_development_community: cost-bearer with jurisdictional exit (powerful / mobile) — bound normatively and politically rather than directly
 *   - patients_denied_interventions: frontline cost-bearer (moderate / constrained) — doctrinal refusal of lawful care
 *   - transhumanist_advocacy_networks: excluded contestant (powerful / constrained) — premises admitted nowhere in the doctrine-setting conversation
 *   - secular_bioethicists: analytical observer (organized / analytical) — documents the structure, binds nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__imago_dei_reading, 0.58).
domain_priors:suppression_score(dignity_kernel__imago_dei_reading, 0.6).
domain_priors:theater_ratio(dignity_kernel__imago_dei_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__imago_dei_reading, mountain).
narrative_ontology:human_readable(dignity_kernel__imago_dei_reading, "Imago Dei Dignity Constraint: Equal Standing Prior to Capability (Technology Governance Reading)").
narrative_ontology:topic_domain(dignity_kernel__imago_dei_reading, "theological ethics / technology governance / philosophical anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__imago_dei_reading).
domain_priors:emerges_naturally(dignity_kernel__imago_dei_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__imago_dei_reading, 'e83a3171-df71-4b25-9c88-2d4ffbdee2eb').
narrative_ontology:cs_kernel_codification('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', formalized).
narrative_ontology:cs_authority_grounding('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', lineage).
narrative_ontology:cs_interpretation_layer_present('e83a3171-df71-4b25-9c88-2d4ffbdee2eb').
narrative_ontology:cs_reading_relation('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', dignity_kernel__autonomy_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', dignity_kernel__posthumanist_reading, forecloses).
narrative_ontology:cs_axiom('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', foundational, imago_dei_confers_inviolable_equal_dignity).
narrative_ontology:cs_axiom_status(imago_dei_confers_inviolable_equal_dignity, holdable).
narrative_ontology:cs_axiom_grounding('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', imago_dei_confers_inviolable_equal_dignity, theological).
narrative_ontology:cs_axiom('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', foundational, created_human_nature_bounds_technological_transformation).
narrative_ontology:cs_axiom_status(created_human_nature_bounds_technological_transformation, holdable).
narrative_ontology:cs_axiom_grounding('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', created_human_nature_bounds_technological_transformation, theological).
narrative_ontology:cs_reference_frame('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', created_order_fixed_human_nature).
narrative_ontology:cs_drift_state('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', contemporary_biotech_ai_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e83a3171-df71-4b25-9c88-2d4ffbdee2eb', '').
narrative_ontology:cs_kernel_id(dignity_kernel__imago_dei_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, magisterial_teaching_authority).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, cognitively_disabled_persons).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, persons_at_margins_of_life).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, religious_healthcare_institutions).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, faith_communities_holding_doctrine).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, persons_in_ai_mediated_care).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, enhancement_seeking_patients).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, human_enhancement_researchers).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, agi_development_community).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, patients_denied_interventions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, persons_in_ai_mediated_care).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, transhumanist_advocacy_networks).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, imago_dei_doctrine).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, created_order_anthropology).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, equal_dignity_prior_to_capability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues binding instruction on reproductive technology, enhancement, end-of-life intervention, and artificial intelligence; forms clergy and healthcare personnel in the doctrine; enforces compliance through institutional policy, canonical discipline, and public advocacy. Its teaching office and its adjudicating role over these questions are constituted by its guardianship of the doctrine — abandoning the kernel would dissolve the office's claim to authority in this domain. It collects institutional continuity, deference, and jurisdictional reach from the constraint's persistence.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, magisterial_teaching_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__imago_dei_reading, magisterial_teaching_authority, beneficiary).

% Live with cognitive impairments that capability-indexed frameworks would discount or exclude. The doctrine secures their standing as equal prior to any capacity: they need not demonstrate rational agency, productivity, or projected quality of life to count. They cannot exit the category their standing depends on; their protection rises and falls with the doctrine's social hold.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, cognitively_disabled_persons, beneficiary,
    powerless, biographical, trapped, global).

% Include the unborn, the profoundly demented, and the persistently unconscious. Their standing under the doctrine does not depend on developmental stage, cognitive presence, or prognosis. Like the disabled, they cannot exit their condition; the doctrine is the load-bearing structure of their protection in law and medicine.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, persons_at_margins_of_life, beneficiary,
    powerless, biographical, trapped, global).

% Operate hospitals, clinics, and research-ethics review under the doctrine: ethics committees refuse interventions the teaching bars, and the institutions' mission identity is constituted by that refusal. They receive mission coherence, community trust, and a distinctive institutional identity from maintaining the constraint, and they administer its day-to-day enforcement at the bedside. Becoming secular would dissolve the identity that organizes them.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, religious_healthcare_institutions, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__imago_dei_reading, religious_healthcare_institutions, agenda_setter).

% Lay and religious members whose moral community is bounded by the shared anthropology: every person counts equally because every person bears the image. The doctrine orders their bioethical self-understanding, their political advocacy, and their intergenerational transmission. Exit means leaving the community's shared moral language, which for many constitutes personal identity.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, faith_communities_holding_doctrine, beneficiary,
    organized, generational, identity_locked, global).

% Patients whose triage, monitoring, and treatment allocation increasingly run through algorithmic systems. The doctrine constrains how far their care may be optimized: they may not be reduced to data points whose standing varies with predicted outcome. They benefit from that floor, and they also bear costs of it — some efficiency and access gains that optimization would deliver are forgone where the constraint binds.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, persons_in_ai_mediated_care, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__imago_dei_reading, persons_in_ai_mediated_care, payer).

% Seek cognitive, physical, or longevity enhancement for themselves or their children. Where the doctrine governs institutions or shapes policy, the interventions they want are categorically barred regardless of safety or consent — not weighed, refused in principle. They can travel to permissive jurisdictions or wait for the doctrine's political hold to weaken, but within governed institutions there is no version of their request that can be granted.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, enhancement_seeking_patients, payer,
    moderate, biographical, constrained, global).

% Work on gene editing for enhancement, neurotechnological augmentation, and radical life extension. The doctrine bars the purpose of their programs in principle: funding channels, institutional review boards, and publication ethics influenced by the teaching close around enhancement aims even where the techniques are permitted for therapy. They can reorient programs toward permitted therapeutic aims or work in permissive jurisdictions; the categorical program itself has no permitted form.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, human_enhancement_researchers, payer,
    organized, generational, constrained, global).

% Build increasingly capable AI systems, some explicitly pursuing general or superhuman intelligence. The doctrine requires that AI remain a tool subordinate to the human person and categorically rejects machine superiority; where its influence reaches, it shapes funding politics, partnership terms, and public legitimacy against their project. Today most of the community operates outside the doctrine's institutional reach and does not recognize its authority — the constraint binds them normatively and politically rather than directly, and they can relocate to jurisdictions and institutions where it holds no sway.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, agi_development_community, payer,
    powerful, civilizational, mobile, global).

% Patients in religiously governed hospitals and clinics who are refused lawful interventions — certain reproductive procedures, some end-of-life options — under policies grounded in the doctrine. Their refusal is not clinical but doctrinal. Exit means finding another institution, sometimes at distance or delay, and the option set was never theirs to shape.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, patients_denied_interventions, payer,
    moderate, biographical, constrained, national).

% Organized movements arguing that enhancement, radical life extension, and ultimately superintelligence are continuous with human flourishing. The doctrine names their program a violation of the created order in principle — not an excess to be regulated but a category error. They contest it in public bioethics and fund parallel institutions, but they are not admitted to the doctrine-setting conversation itself, where their premises are treated as errors rather than positions.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, transhumanist_advocacy_networks, payer,
    powerful, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__imago_dei_reading, transhumanist_advocacy_networks, excluded).

% Scholars and advisory-body members who analyze the doctrine's operation in medicine and technology policy. They document its institutional reach, its conflicts with autonomy-based frameworks, and its protective effects; they deliberate in the public bioethics sphere but hold no seat in the magisterium's internal adjudication, and their analyses bind nothing.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, secular_bioethicists, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__imago_dei_reading, magisterial_teaching_authority).
narrative_ontology:fixing_cost_class(dignity_kernel__imago_dei_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the standing problem: provides a shared, capability-independent criterion — every human counts equally prior to any capacity, productivity, developmental stage, or prognosis — so the moral community's boundary does not have to be re-litigated per case and institutions can act on a settled floor when capability sorting would exclude the powerless. In technology governance it fixes a single boundary — the human person as the limit for tool-building — that otherwise each lab, clinic, and market would set for itself.
% TRANSFER_FUNCTION: Transfers categorical decision authority over enhancement, reproductive technology, end-of-life intervention, and AI development from individuals, researchers, and developers to the teaching authority and the institutions administering its policy. Transfers protection — standing, care, legal advocacy, refusal of reduction — to persons at the margins of capability. Transfers compliance costs to enhancement seekers, enhancement researchers, the advanced-AI development community where the doctrine's influence reaches, and patients in governed institutions who are refused lawful interventions.
% ABSENT_VOICES: Enhancement-seeking patients, transhumanist advocates, and AGI developers deliberate nowhere the doctrine is set: the magisterium's internal fora admit only their own premises, and rival objections arrive pre-classified as errors. Patients refused interventions in religious hospitals appear as cases to be ruled on, not as deliberators. Secular bioethicists engage in the public sphere but hold no seat in the adjudicating structure. The unanimity of the doctrine-setting conversation is therefore partly an artifact of who was never admitted — the consensus-provenance check should read this constraint's internal agreement accordingly.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, capability-indexed frameworks would fill the vacuum within a generation of case law and allocation policy: standing for the unborn, the demented, and the cognitively disabled would be renegotiated on contested functional criteria; religious healthcare institutions would re-found their ethics on autonomy or stewardship grounds and the refusal practices would change; enhancement governance would proceed without a categorical bar, contested only by safety and consent regulation; and the teaching authority would lose the adjudicating role the doctrine constitutes. The world rearranges because every seat in the structure holds its position by reference to the constraint.
% FOUNDING_PROBLEM: Securing unconditional standing for every human against capability sorting — historically: exposure of infants, denial of personhood to the disabled and the enslaved, eugenic sterilization. The doctrine was forged to make standing prior to and independent of capacity, usefulness, or stage of development, so that no argument about capability could ever remove a person from the moral community.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set on the problem's liveness, not on the doctrine's explanation: disability-rights scholarship and the UN Convention on the Rights of Persons with Disabilities attest that standing independent of capacity is a live requirement; historians of eugenics and of mass institutionalization attest the historical capability sorts the doctrine was forged against; health-allocation literature on QALY-based rationing and deployed algorithmic-triage systems attest that capability sorting is intensifying. No party outside the beneficiary set corroborates the theological account — divine-image conferral — itself; the corroboration covers the founding problem's existence and persistence, which is what the status claim requires.
narrative_ontology:disappearance_verdict(dignity_kernel__imago_dei_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__imago_dei_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__imago_dei_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignity_kernel__imago_dei_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__imago_dei_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__imago_dei_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__imago_dei_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, ExtMetricName, E),
    domain_priors:suppression_score(dignity_kernel__imago_dei_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dignity_kernel__imago_dei_reading),
    narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dignity_kernel__imago_dei_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is mountain because this reading asserts the constraint as created order — inviolable, conferred, prior to any capability, not a human artifact — and emerges_naturally is authored true on that basis. The metrics are authored from the constraint's documented operation, which is not mountain-shaped: enforcement is active and continuous (magisterial instruction, institutional ethics committees, funding politics, public advocacy), the constraint is contested at scale, and alternatives persist outside governed institutions. Extractiveness 0.58: the categorical bars impose total, capability-independent costs on identifiable parties while conferring genuine protection on the powerless; the cost-bearing population is narrower than in confiscatory constraints, but the bars are absolute rather than calibrated. Suppression 0.60, authored as a raw structural property and unscaled by power or scope in the engine's arithmetic: a mix of structural enforcement (institutional policy, funding closure) and formational internalization. Theater ratio 0.20: the doctrine performs real refusals and real protections; restatement documents are maintenance of a live function, not performance of a dead one. Accessibility collapse 0.40: within governed institutions alternatives are foreclosed in principle, but secular bioethics, permissive jurisdictions, and rival frameworks keep the field open. Resistance 0.60: sustained contestation from bioethics, affected professions, and affected patients. The measurement series share one seven-point grid (1978-2026) and all three tracked metrics are authored at every point; the rising base_extractiveness series models extraction accumulating as each new technology (IVF, embryo experimentation, enhancement, germline editing, advanced AI) extends the categorical bars to new cost-bearing parties. Identity-lock concentrates in the agenda-setting and beneficiary-institution seats: the teaching office has become its guardianship function, religious healthcare institutions' mission identity is constituted by doctrinal refusal, and for faith communities the shared anthropology is the moral language itself — exit equals self-dissolution, which is why enforcement costs stay low inside the perimeter and high at its edge. If the identity frames broke — a teaching office that re-founded authority on service rather than guardianship, hospitals that secularized — the constraint's enforcement surface would collapse to advocacy alone and its effective suppression would fall sharply.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the teaching authority's seat the constraint is guardianship of a conferred status: enforcement is protection, the categorical bars are boundary-keeping, and the cost-bearers are attempting what cannot be owned. From the enhancement seeker's or refused patient's seat the same structure is an unconsented authority barring their life-projects in principle — not weighed, refused as a category. From the disabled person's seat the constraint is the difference between standing and nonexistence: under capability-indexed frameworks their protection is contingent on arguments they must win; under this one it is prior to argument. The engine computes these divergences from the power and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the protected classes (disabled persons, margins of life, AI-mediated care patients) are subsidized by the constraint — it confers standing they could not otherwise secure — and their trapped exit does not raise d because the constraint's costs to them are diffuse optimization forgone, not extraction. The teaching authority sits at the beneficiary end despite running the enforcement: it collects authority, deference, and jurisdictional reach from the constraint's persistence. Cost-bearers drive high directionality: enhancement seekers and doctrinally refused patients (constrained exit, immediate stakes) sit near the full-target end; enhancement researchers (organized, able to reorient programs) somewhat lower; the AGI community lower still — its mobile exit means the constraint binds it normatively and politically rather than directly, so effective extraction is damped by arbitrage across jurisdictions. Suppression is a raw structural property and is not scaled; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing unconditional standing against capability sorting — is live and arguably more urgent than at the doctrine's forging: algorithmic triage, QALY rationing, enhancement markets, and cognitive-elitist politics all re-run the capability sort at scale. No mandatrophy is declared. The classification work this story enables is guarding against the reverse error: mislabeling a constraint with a real protective floor as pure extraction, or mislabeling its categorical bars as pure coordination. The structural data — named beneficiaries, named cost-bearers, active enforcement, live resistance — let the engine hold both truths at once: the floor is genuine coordination whose beneficiaries include the least powerful parties in the structure, and the bars are enforced extraction whose costs fall on identifiable, mostly non-powerless parties. The mismatch consumer should note founding_problem_status 'live' paired with disappearance_verdict 'world_rearranges' — no capture or zombie flag is expected on this pairing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_institutional_construction,
    'Is the equal-dignity-prior-to-capability constraint a genuine feature of created reality, as the reading holds, or a constructed constraint whose persistence serves identifiable institutional interests?',
    'Comparative-institutional analysis: measure whether capability-independent protections for the margins of life emerge and persist in non-theological frameworks at comparable strength, and whether the theological constraint outperforms them where both operate in the same jurisdiction; natural-experiment evidence from jurisdictions where the doctrine''s institutional carriers lost enforcement capacity.',
    'If constructed, the reading''s mountain self-claim fails and the constraint classifies by its enforced, beneficiary-bearing operation; if genuine natural law, enforcement is guardianship rather than maintenance and the categorical prohibitions carry the warrant of the created order itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_or_institutional_construction, conceptual, 'Whether the dignity constraint is natural law or institutional construction — the irreducible ambiguity behind the mountain claim.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the imago_dei_reading of the dignity_kernel: which of its structural elements are reading-indexed rather than kernel-fixed, and what would the sibling readings (autonomy_rights_reading, posthumanist_reading) change?',
    'Cross-reading comparison of the dignity_kernel family stories: beneficiary sets, victim sets, categorical scope, and enforcement surfaces, read against the shared kernel commitment that human dignity is inviolable and equal.',
    'The autonomy reading would dissolve the categorical enhancement bar (self-modification as rights-exercise) and shrink the protected class at the capability margins; the posthumanist reading would invert the structure entirely, making the constrained the victims and transformation a flourishing-continuum. The disagreement is located at two points: the ground of dignity (conferred divine image vs. constituted autonomy vs. no ground needed) and whether the human limit is fixed. This story''s epsilon, victims, and beneficiaries are valid only under this reading''s answers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-indexed elements of the dignity kernel and the located disagreement with sibling readings.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the constraint''s suppression of enhancement-seeking and enhancement research primarily structural (institutional policy, funding channels, review-board closure) or internalized (formed conscience that no longer desires the barred goods)?',
    'Post-exit trajectory of members who leave the framework: whether enhancement desires and research ambitions re-emerge once formational pressure is removed; comparative rates of barred-goods seeking inside versus outside governed institutions.',
    'If largely internalized, the constraint''s coercive overhead is lower than the structural measure suggests — formation performs the enforcement and the suppression metric overstates external coercion; if structural, the enforcement machinery is load-bearing and its cost persists at full weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression operates through external enforcement or through formation that internalizes the limit.').

omega_variable(
    protection_prohibition_separability,
    'Are the constraint''s protective function (equal standing prior to capability) and its prohibitive function (categorical rejection of enhancement and superintelligence) structurally separable, or does the protection depend on the fixed-nature premise that generates the prohibitions?',
    'Comparative analysis of secular frameworks that secure capability-independent standing (disability-rights-based, rights-based) without a categorical enhancement bar: do they protect the margins at comparable strength while permitting enhancement?',
    'If separable, the prohibitions are extraction riding on a coordination floor that could persist without them, and reform could strip the extraction while keeping the protection; if inseparable, the prohibitions are the price of the protection and the measured extraction overstates removable overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_prohibition_separability, conceptual, 'Whether the equal-dignity floor and the categorical technological prohibition can be structurally decoupled.').

omega_variable(
    victim_set_referent_ambiguity,
    'The reading''s declared victim set — persons subjected to technocratic reduction or transhumanist transformation — names the constraint''s protected class, while the structural cost-bearers of this constraint are enhancement seekers, researchers, and doctrinally refused patients. Which set governs the constraint''s structural grammar?',
    'Derivation discipline: base_properties.victims feeds directionality as cost-bearers; the reading''s protective class is authored under beneficiaries; this omega records that the reading''s own victim language inverts the structural grammar and must not be read as cost-bearing.',
    'Reading the declared victim set as cost-bearers would assign high directionality to the powerless protected seats, invert d across the structure, and flip the computed classification; keeping the referents distinct preserves the protective-floor-plus-categorical-cost structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_referent_ambiguity, conceptual, 'Referent ambiguity between the reading''s declared victim language and the constraint''s structural cost-bearers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__imago_dei_reading, 1978, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1978, dignity_kernel__imago_dei_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement_basis(dign_tr_t1978, observed).
narrative_ontology:measurement(dign_tr_t1987, dignity_kernel__imago_dei_reading, theater_ratio, 1987, 0.16).
narrative_ontology:measurement_basis(dign_tr_t1987, observed).
narrative_ontology:measurement(dign_tr_t1995, dignity_kernel__imago_dei_reading, theater_ratio, 1995, 0.17).
narrative_ontology:measurement_basis(dign_tr_t1995, observed).
narrative_ontology:measurement(dign_tr_t2003, dignity_kernel__imago_dei_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement_basis(dign_tr_t2003, observed).
narrative_ontology:measurement(dign_tr_t2009, dignity_kernel__imago_dei_reading, theater_ratio, 2009, 0.19).
narrative_ontology:measurement_basis(dign_tr_t2009, observed).
narrative_ontology:measurement(dign_tr_t2017, dignity_kernel__imago_dei_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement_basis(dign_tr_t2017, observed).
narrative_ontology:measurement(dign_tr_t2026, dignity_kernel__imago_dei_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement_basis(dign_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t1978, dignity_kernel__imago_dei_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement_basis(dign_be_t1978, observed).
narrative_ontology:measurement(dign_be_t1987, dignity_kernel__imago_dei_reading, base_extractiveness, 1987, 0.47).
narrative_ontology:measurement_basis(dign_be_t1987, observed).
narrative_ontology:measurement(dign_be_t1995, dignity_kernel__imago_dei_reading, base_extractiveness, 1995, 0.51).
narrative_ontology:measurement_basis(dign_be_t1995, observed).
narrative_ontology:measurement(dign_be_t2003, dignity_kernel__imago_dei_reading, base_extractiveness, 2003, 0.54).
narrative_ontology:measurement_basis(dign_be_t2003, observed).
narrative_ontology:measurement(dign_be_t2009, dignity_kernel__imago_dei_reading, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement_basis(dign_be_t2009, observed).
narrative_ontology:measurement(dign_be_t2017, dignity_kernel__imago_dei_reading, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement_basis(dign_be_t2017, observed).
narrative_ontology:measurement(dign_be_t2026, dignity_kernel__imago_dei_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(dign_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1978, dignity_kernel__imago_dei_reading, suppression_requirement, 1978, 0.45).
narrative_ontology:measurement_basis(dign_su_t1978, observed).
narrative_ontology:measurement(dign_su_t1987, dignity_kernel__imago_dei_reading, suppression_requirement, 1987, 0.5).
narrative_ontology:measurement_basis(dign_su_t1987, observed).
narrative_ontology:measurement(dign_su_t1995, dignity_kernel__imago_dei_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(dign_su_t1995, observed).
narrative_ontology:measurement(dign_su_t2003, dignity_kernel__imago_dei_reading, suppression_requirement, 2003, 0.57).
narrative_ontology:measurement_basis(dign_su_t2003, observed).
narrative_ontology:measurement(dign_su_t2009, dignity_kernel__imago_dei_reading, suppression_requirement, 2009, 0.58).
narrative_ontology:measurement_basis(dign_su_t2009, observed).
narrative_ontology:measurement(dign_su_t2017, dignity_kernel__imago_dei_reading, suppression_requirement, 2017, 0.59).
narrative_ontology:measurement_basis(dign_su_t2017, observed).
narrative_ontology:measurement(dign_su_t2026, dignity_kernel__imago_dei_reading, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(dign_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__imago_dei_reading, identity_coordination).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, dignity_kernel__autonomy_rights_reading).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% 'Human dignity' decomposes into three readings of the dignity_kernel, each a structurally distinct constraint with its own epsilon: this imago-dei arrangement (conferred, capability-independent standing; categorical technological limits), the autonomy-rights arrangement (dignity grounded in autonomy and rights; enhancement as rights-exercise), and the posthumanist arrangement (no fixed limit; transformation as flourishing). The epsilon values differ because the victim sets and categorical scopes differ; the family link records that the readings compete over one kernel and that this reading's institutional enforcement changes the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
