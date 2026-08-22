% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Constraint on Constitutional Interpretation (US Constitution, 1787)
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the
 *   us_constitution_1787 kernel as a single ε-invariant constraint: the
 *   standing arrangement under contest is the fixed-meaning interpretive
 *   order itself — the practice of deriving constitutional commands from the
 *   text's public meaning at ratification, with the framers' intent binding
 *   on interpreters and change passing only through Article V. The
 *   arrangement is enforced not by any single administrator but through a
 *   distributed apparatus: judicial appointment pipelines screened for
 *   fidelity, confirmation and primary-election sanctions, law-school
 *   curricula, a scholarly industry of founding-era evidence, and the
 *   legitimacy costs of deciding otherwise. The kernel context's expected
 *   structural delta is visible in the party structure: a narrow constraint
 *   set (only what ratification-era meaning reaches), pre-1787 police-power
 *   practices left presumptively legitimate (the state sovereignty seat),
 *   modern social-rights claims falling outside the boundary (the claimant
 *   seats), and high epistemic demands on historical evidence (the
 *   resource-constrained litigant seat). Per the ε-referent rule for kernel
 *   readings, extractiveness (0.22) is authored for THIS arrangement as the
 *   reading's own lights assess it — the reading counts the discipline it
 *   imposes as the price of written law, not extraction — while the
 *   beneficiary/victim structure is authored descriptively from the
 *   arrangement's actual operation. The engine computes per-seat
 *   classifications from that structure; the divergence between the
 *   reading-indexed ε and the payer seats' computed extraction is the
 *   measurement this story exists to take. Sibling readings (living_reading,
 *   positivist_reading) are separate constraint stories, not parts of this
 *   one. KEY AGENTS (by structural relationship): -
 *   originalist_legal_establishment: agenda-setter and principal beneficiary
 *   (institutional/identity_locked) — administers the method and collects its
 *   authority, careers, and judicial power -
 *   conservative_appointment_coalition: beneficiary (powerful/mobile) — uses
 *   fidelity as a durable selection and mobilization criterion -
 *   property_and_arms_interests: beneficiary (organized/constrained) — holds
 *   expressly enumerated rights insulated against evolving-rights dilution -
 *   state_sovereignty_interests: beneficiary (institutional/constrained) —
 *   shielded from an evolving national rights baseline -
 *   modern_equality_claimants: primary target (moderate/trapped) — claims
 *   outside the 1787/1791 frame fail under fixed meaning -
 *   unenumerated_rights_claimants: target (powerless/trapped) —
 *   evolving-liberty claims lack founding-era anchors -
 *   resource_constrained_litigants: target (powerless/trapped) — bear the
 *   high epistemic burden against better-funded opponents -
 *   non_originalist_jurists: target (institutional/identity_locked) — bear
 *   legitimacy sanctions for rival-method adjudication - legal_historians:
 *   analytical observer — audits the historical record the method consumes -
 *   future_generations: excluded and bearing costs (powerless/trapped) —
 *   bound without consent and without a seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.22).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.55).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Constraint on Constitutional Interpretation (US Constitution, 1787)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, 'bc22e52b-d579-49b4-a03d-7fa1d3eba4b8').
narrative_ontology:cs_kernel_codification('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', fixed_text).
narrative_ontology:cs_authority_grounding('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', lineage).
narrative_ontology:cs_interpretation_layer_present('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8').
narrative_ontology:cs_reading_relation('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', us_constitution_1787__positivist_reading, forecloses).
narrative_ontology:cs_axiom('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', foundational, ratification_fixes_constitutional_meaning).
narrative_ontology:cs_axiom_status(ratification_fixes_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', ratification_fixes_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', foundational, framers_intent_binds_interpreters).
narrative_ontology:cs_axiom_status(framers_intent_binds_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', framers_intent_binds_interpreters, deontological).
narrative_ontology:cs_axiom('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', secondary, judicial_meaning_updating_illegitimate).
narrative_ontology:cs_axiom_status(judicial_meaning_updating_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', judicial_meaning_updating_illegitimate, instrumental).
narrative_ontology:cs_reference_frame('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', ratification_era_fixed_meaning).
narrative_ontology:cs_drift_state('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', contemporary_post_dobbs_bruen, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('bc22e52b-d579-49b4-a03d-7fa1d3eba4b8', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_legal_establishment).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_appointment_coalition).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, property_and_arms_interests).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, state_sovereignty_interests).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_equality_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, resource_constrained_litigants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, non_originalist_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, future_generations).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, popular_sovereignty_through_fixed_enactment).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_legitimacy_through_restraint).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, written_constitution_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges, law professors, think-tank scholars, and practitioners who developed and administer the fixed-meaning method: they train its practitioners, staff its journals, screen judicial nominees for fidelity to it, and write the opinions that apply it. The method supplies their professional standing, career paths, and institutional power; a leading figure who renounced it would lose standing in the networks that built their career. They run the arrangement, and its returns — appointments, authority, and the market for founding-era expertise — land with them.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_legal_establishment, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, originalist_legal_establishment, beneficiary).

% Political actors, interest groups, and donors who select and confirm judges on demonstrated fidelity to fixed meaning. The criterion gives them a durable, legible test that survives individual nominees, organizes primary electorates, and delivers a bench that rules as they would wish without case-by-case negotiation. If the criterion lost its value they could adopt another; their position is chosen, not bound.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_appointment_coalition, beneficiary,
    powerful, generational, mobile, national).

% Holders of rights the 1791 text expressly names — arms, property, contract, criminal-procedure protections — whose holdings are insulated because the governing meaning does not move with contemporary opinion. Regulatory campaigns against them must defeat the historical record rather than shift judicial values. They did not create the arrangement; they hold protected positions under it and defend it when challenged.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, property_and_arms_interests, beneficiary,
    organized, generational, constrained, national).

% State governments whose reserved police powers and pre-ratification institutional practices remain presumptively legitimate because the governing meaning does not expand with national opinion. An evolving-rights baseline would subject more state choices to federal override; fixed meaning keeps the floor where the founding generation set it. They cannot leave the constitutional order; they defend the arrangement through litigation and appointment politics.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, state_sovereignty_interests, beneficiary,
    institutional, generational, constrained, national).

% People claiming protection against discrimination on bases the 1787/1791 polity did not contemplate as equality concerns — sex, sexual orientation, digital-age surveillance, disability in its modern legal sense. Their claims succeed only if the historical record supports them or an amendment passes; otherwise they lose regardless of contemporary consensus. They cannot exit the legal order; their remedies are to fund historical research, litigate for decades, or pursue an amendment that has succeeded only twenty-seven times.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_equality_claimants, payer,
    moderate, biographical, trapped, national).

% People whose claims rest on liberty understandings that have developed since ratification — privacy in contraception, intimacy, travel, bodily autonomy. Under fixed meaning these claims must find their anchor in founding-era law or fail. Many cannot identify a founding-era anchor because the practices they seek to protect were not contested then in their modern form. They bear the outcome and cannot opt out of the legal order that produces it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Litigants without institutional funding who must contest constitutional questions using founding-era dictionaries, corpora, and archival records. State attorneys general and well-funded repeat players field teams of historians; an indigent defendant or small organization fields a brief. The method converts every constitutional question into a research contest, and research capacity is unequally distributed. They bear that burden in every case they cannot resource.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, resource_constrained_litigants, payer,
    powerless, immediate, trapped, national).

% Judges and scholars who decide constitutional questions on premises other than fixed ratification-era meaning — moral reading, common-law adaptation, pragmatic balancing. They face confirmation defeat, summary reversal, professional delegitimation, and movement-funded primary challenges against allied politicians. Their jurisprudence is their career; renouncing it ends their standing in the communities that sustain them, while persisting in it carries escalating sanction.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, non_originalist_jurists, payer,
    institutional, generational, identity_locked, national).

% Academic historians of the founding era whose work supplies — and audits — the evidence the method consumes. They publish findings that both support and undercut particular originalist claims, and they document where judicial use of history diverges from the professional consensus of their own discipline. They collect no returns from the arrangement and bear none of its costs; their seat is analytical.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, legal_historians, observer,
    analytical, generational, analytical, national).

% People not yet born who will live under meaning fixed in 1787/1791 as amended. They cannot consent, cannot participate in the interpretive coalition, and their only formal channel — amendment — requires supermajorities they will inherit rather than choose. What flows to them is a completed legal settlement they had no part in making; what flows from them is compliance.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, future_generations, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, future_generations, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_legal_establishment).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a continental republic of 330 million coordinates on a single supreme law across centuries: it gives judges, officials, and citizens one shared decision procedure — derive constitutional commands from the text's fixed public meaning at ratification — instead of from each interpreter's moral or policy judgment. It disciplines judicial discretion by sourcing decisions outside the judge's own values, and it routes legal change through the amendment process rather than judicial revision.
% TRANSFER_FUNCTION: Moves interpretive authority and outcome control from present-day majorities and their litigants — whose claims must fit 1787/1791 meaning or fail — toward the founding polity's judgments and toward those skilled and resourced enough to reconstruct them: originalist-trained judges, well-funded repeat players, and the founding-era historical-argument profession. It also moves legitimacy: decisions rendered in fixed-meaning terms carry the aura of enacted law rather than judicial preference.
% ABSENT_VOICES: Future generations, who will live under fixed meaning without having consented and hold no seat in the interpretive coalition; the descendants of the enslaved, of women, and of the unpropertied — excluded from the 1787 ratifying polity — whose equal standing now depends on meaning the ratifiers did not hold; and non-originalist jurists, whose premises the method treats as illegitimate rather than as a rival reading to be answered. Each would object that the binding is to judgments in which they had no share; they are outside the room because the method recognizes no standing for the objection.
% DISAPPEARANCE_RATIONALE: A decade of precedents decided in fixed-meaning terms, a judicial appointment pipeline screened on fidelity, law-school curricula, and a scholarly industry of founding-era evidence are organized around the method. Overnight removal would leave courts without their stated decision procedure, reopen recently decided lines of cases, force the appointment coalition to find a new criterion, and strand the historical-evidence profession.
% FOUNDING_PROBLEM: How can judicial review be legitimate in a democracy — how may unelected judges invalidate acts of elected majorities without imposing their own values? The originalist answer: judges apply law the people enacted, whose meaning was fixed at ratification; updating meaning is amendment's job, not theirs. A second, supporting problem: how a vast, heterogeneous republic keeps one Constitution as a common reference point across generations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the countermajoritarian difficulty was named and developed by critics of judicial review — Alexander Bickel, no originalist — and is acknowledged across the methods spectrum; living-constitutionalist theorists (John Hart Ely's representation-reinforcement) built their own answers to the same problem, which attests the problem rather than this reading's answer. No serious participant in the methods debate disputes that the problem is live; what is disputed is whether fixed meaning solves it.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).
:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.22 as the reading-indexed value: from the originalist seat the arrangement's costs — the discipline judges accept, the claims that fail for want of a founding-era anchor, the research burden litigants carry — are the legitimate price of enacted law, acknowledged but not counted as extraction. The series rises over the interval because even on the reading's own lights the domain of life governed by fixed meaning has widened, from academic argument in 1971 to controlling doctrine in several fields by 2025. Suppression (0.55) is a raw structural property, unscaled by scope or power: the arrangement is held by appointment-litmus machinery, confirmation defeat, reversal, primary challenges, and professional delegitimation — real coercive infrastructure, though rival methods remain lawful and practiced, so it is not total. Theater_ratio (0.42): a genuine methodological apparatus exists (corpus linguistics, founding-era dictionaries, district-level historical work), but a substantial share of judicial historicism is outcome-driven 'law office history' — selective citation assembled after the conclusion is chosen — and that share has grown as the method became an appointment criterion. Accessibility_collapse (0.4): rival methods do not collapse on contact — living and pragmatist adjudication remain available and practiced, which is precisely why the enforcement machinery must be maintained at all. Resistance (0.65): the methods war has run for five decades, the academic majority opposed the method for most of it, and political backlash to fixed-meaning rulings recurs. Claimed type tangled_rope, authored independently of the metrics: the arrangement possesses a genuine coordination function (one shared decision procedure, judicial discipline, a legitimacy channel routing change through amendment) AND asymmetric extraction (the living bound to founding judgments they did not consent to; claimant classes converted into historical-research contestants; benefits concentrated in the establishment and the interests it insulates), held together by the active enforcement the measurements show being built up across the interval. All three tracked metric series share one time grid (T=0..54) so no metric's end-state is backfilled into earlier points.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat computes a different arrangement than the payer seats do. From the establishment seat the structure is law's discipline: fidelity, stability, legitimacy, a republic governed by enacted text rather than judicial values. From the claimant seats the same structure is a barrier that converts their constitutional claims into research contests against better-funded opponents and renders contemporary consensus irrelevant. From the rival-jurist seat it is a legitimacy machine that sanctions their method's practitioners. The payer classes' coalition potential is real but fragmented: equality claimants, privacy claimants, and rival jurists do not naturally coalesce, and the establishment's identity lock makes internal defection costly — which is why the arrangement persists despite the breadth of the payer side. The engine computes these per-seat classifications from the authored power/exit/role data; the divergence is the finding, not something the claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the establishment (which also administers — the agenda-setter seat), the appointment coalition (mobile exit, chosen position), the property-and-arms interests, and the state-sovereignty interests; the arrangement subsidizes all four. The victim declarations drive high directionality for the claimant classes (trapped exit — no exit from the legal order; amendment is the only channel and it is prohibitively hard), for rival jurists (identity-locked — sanction for persisting, ruin for renouncing), and for future generations (excluded and bound). Resource-constrained litigants sit at the full-target end despite their procedural innocence: the epistemic burden falls on them without their having chosen the method that creates it. Spatial scope is national, and verification of 'original meaning' is contestable at that scope, which amplifies effective extraction at the payer seats; the beneficiary seats are insulated because they control the verification standard itself. Suppression, again, is authored unscaled; only extractiveness carries the directionality and scope scaling in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how judicial review can be legitimate in a democracy — is live, so there is no mandatrophy to resolve and no zombie flag: founding_problem_status=live paired with disappearance_verdict=world_rearranges is the honest combination (machinery is organized around the arrangement because the problem it answers is still contested). The classification's work here is boundary-keeping against mislabels in both directions. A pure-coordination reading — the method's own self-presentation, and the implication of the reading-indexed ε alone — would miss the asymmetric extraction the payer seats bear and certify the arrangement as a rope. A pure-extraction reading — the rival polemic that treats fixed meaning as mere entrenchment of founding-era power — would miss the genuine coordination function that even opponents rely on when they want constitutional law to be determinate at all. Tangled rope holds both: coordination and extraction through the same structure, sustained by the enforcement machinery whose buildup the suppression series records.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_sibling_readings,
    'This constraint is the originalist_reading of the us_constitution_1787 kernel; what would each sibling reading change structurally if adopted in place of this one?',
    'Compare the linked sibling stories (us_constitution_1787__living_reading, us_constitution_1787__positivist_reading): the disagreement is located in what binds the interpreter — fixed ratification-era intent (this reading), evolving societal meaning (living reading), or the enacted text plus democratic amendments alone (positivist reading).',
    'Under the living reading the payer classes here (modern equality and unenumerated-rights claimants) gain standing and the beneficiary classes lose their insulation; under the positivist reading the founding-era evidence industry and the intent-inquiry machinery lose their role and the epistemic burden on litigants largely disappears. The beneficiary/victim structure authored here is specific to this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_sibling_readings, conceptual, 'Committer structure: kernel membership, reading identity, and the structural delta each sibling would produce.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel the constitutional text itself, the popular-sovereignty legitimacy claim layered above the text, or the interpretive tradition that has grown around both — and does the choice change the classification?',
    'Test the story under the alternative framings: a text-anchored kernel makes fixed-meaning adjudication a reading of an enactment; a tradition-anchored kernel makes this reading one node in a lineage of interpretive practice, with practice_drift rather than revival_pressure as the natural drift direction.',
    'If the tradition-anchored framing is adopted, the drift_state declared here (revival_pressure) would re-author as practice_drift, and the reading_relations to the siblings would loosen from logical foreclosure toward lineage divergence — changing the computed terminal attractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel (the text) is the only defensible framing, or the legitimacy claim and tradition above it are equally defensible.').

omega_variable(
    historical_evidence_indeterminacy,
    'Is the founding-era record determinate enough to bind — and where it is silent or contested, does the method deliver law or judicial discretion dressed as history?',
    'Corpus-linguistic and founding-era-usage databases; audit of decided cases against the professional consensus of founding-era historians; track the rate at which contested historical questions are resolved against the method''s own canons.',
    'If the record is too indeterminate at the points of frequent litigation, theater_ratio rises and the arrangement''s operation shifts from principled binding toward discretionary gatekeeping — moving payer-seat classifications toward the extractive end without any change in the declared method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_indeterminacy, empirical, 'Whether the high epistemic demands the method imposes can actually be met by the historical record.').

omega_variable(
    dead_hand_consent_legitimacy,
    'Does supermajoritarian consent given in 1787/1791 — by a polity that excluded enslaved people, women, and the unpropertied — legitimately bind the present, or is the binding imposition without consent?',
    'Not resolvable by data alone: it turns on the weight given to intergenerational political obligation versus present-day consent. Article V amendment rates and the inclusiveness of the contemporary amendment coalition are the relevant observable margins.',
    'If the founding consent is judged defective and Article V too rigid to supply ongoing consent, the payer classes bear nonconsensual imposition and their seat classifications move toward the pure-extraction end; if Article V supplies adequate ongoing consent, the binding is legitimate coordination cost and the payer-seat classifications soften.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_consent_legitimacy, preference, 'Whether the binding of the living is consented-to law or nonconsensual imposition — the classic dead-hand question.').

omega_variable(
    enforcement_vs_competition_boundary,
    'Is the appointment-and-sanction machinery that enforces fixed-meaning fidelity ordinary political competition over judicial philosophy, or suppression of interpretive alternatives?',
    'Measure the institutional space remaining to rival-method jurists: state-court adoption, federal district diversity, academic hiring, and whether sanction tracks the reasoning of decisions or only their outcomes.',
    'If rival methods retain genuine institutional space, the machinery reads as competition within a plural legal culture and payer-seat extraction stays moderated; if the space is closing, the machinery reads as alternative-suppression and the arrangement moves toward the pure-extraction end for the jurist and claimant seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_competition_boundary, empirical, 'Whether enforcement of the method is legitimate competition or suppression of alternatives — the coordination/extraction boundary question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t9, us_constitution_1787__originalist_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t9, observed).
narrative_ontology:measurement(us_c_tr_t18, us_constitution_1787__originalist_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t18, observed).
narrative_ontology:measurement(us_c_tr_t27, us_constitution_1787__originalist_reading, theater_ratio, 27, 0.31).
narrative_ontology:measurement_basis(us_c_tr_t27, observed).
narrative_ontology:measurement(us_c_tr_t36, us_constitution_1787__originalist_reading, theater_ratio, 36, 0.34).
narrative_ontology:measurement_basis(us_c_tr_t36, observed).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_1787__originalist_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement_basis(us_c_tr_t45, observed).
narrative_ontology:measurement(us_c_tr_t54, us_constitution_1787__originalist_reading, theater_ratio, 54, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t54, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t9, us_constitution_1787__originalist_reading, base_extractiveness, 9, 0.14).
narrative_ontology:measurement_basis(us_c_be_t9, observed).
narrative_ontology:measurement(us_c_be_t18, us_constitution_1787__originalist_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement_basis(us_c_be_t18, observed).
narrative_ontology:measurement(us_c_be_t27, us_constitution_1787__originalist_reading, base_extractiveness, 27, 0.17).
narrative_ontology:measurement_basis(us_c_be_t27, observed).
narrative_ontology:measurement(us_c_be_t36, us_constitution_1787__originalist_reading, base_extractiveness, 36, 0.19).
narrative_ontology:measurement_basis(us_c_be_t36, observed).
narrative_ontology:measurement(us_c_be_t45, us_constitution_1787__originalist_reading, base_extractiveness, 45, 0.21).
narrative_ontology:measurement_basis(us_c_be_t45, observed).
narrative_ontology:measurement(us_c_be_t54, us_constitution_1787__originalist_reading, base_extractiveness, 54, 0.22).
narrative_ontology:measurement_basis(us_c_be_t54, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t9, us_constitution_1787__originalist_reading, suppression_requirement, 9, 0.28).
narrative_ontology:measurement_basis(us_c_su_t9, observed).
narrative_ontology:measurement(us_c_su_t18, us_constitution_1787__originalist_reading, suppression_requirement, 18, 0.36).
narrative_ontology:measurement_basis(us_c_su_t18, observed).
narrative_ontology:measurement(us_c_su_t27, us_constitution_1787__originalist_reading, suppression_requirement, 27, 0.42).
narrative_ontology:measurement_basis(us_c_su_t27, observed).
narrative_ontology:measurement(us_c_su_t36, us_constitution_1787__originalist_reading, suppression_requirement, 36, 0.48).
narrative_ontology:measurement_basis(us_c_su_t36, observed).
narrative_ontology:measurement(us_c_su_t45, us_constitution_1787__originalist_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement_basis(us_c_su_t45, observed).
narrative_ontology:measurement(us_c_su_t54, us_constitution_1787__originalist_reading, suppression_requirement, 54, 0.55).
narrative_ontology:measurement_basis(us_c_su_t54, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'what the Constitution means and how it binds' decomposes, per the ε-invariance principle, into three structurally distinct constraints — this fixed-meaning reading, the living reading (us_constitution_1787__living_reading), and the positivist reading (us_constitution_1787__positivist_reading). Each has its own ε (this one reading-indexed to the originalist seat), its own beneficiary/victim structure, and its own enforcement machinery; forcing them into one story would make ε observer-relative. This file links both siblings as family members of the shared kernel (the text's authority). Environmental coupling — this reading's appointment-pipeline success changes the bench composition in which the sibling readings are practiced — is recorded here as network edges; the logical relations between readings are recorded separately in cs_structure.reading_relations, where this reading's core premises directly contradict both siblings' core premises within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
