% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Death (Consent-Irrelevant)
 *   domain: bioethics/medical law/political philosophy
 *
 * SUMMARY:
 *   A broad family of criminal statutes, professional-sanction regimes, and
 *   constitutional doctrines prohibit intentional life-termination - assisted
 *   dying, voluntary euthanasia, and in their strongest forms any killing
 *   absent a narrow legal justification - on the ground that life's value is
 *   intrinsic and that consent cannot license its violation. The arrangement
 *   is presented as the protection of the vulnerable and the preservation of
 *   a civilizational floor; its critics read the same arrangement as the
 *   coercive prolongation of suffering imposed on the dying, the disabled,
 *   the elderly, and the poor, enforced against supermajority public
 *   preference by criminal penalty and professional ruin. This story
 *   instantiates the sanctity_primary reading of the dignified_death kernel:
 *   the prohibition itself is the standing arrangement under contest, and
 *   epsilon (0.60) is authored for THAT arrangement as its critical structure
 *   presents it - concentrated suffering borne by identifiable populations,
 *   diffuse and immaterial gains. CONSTRAINT FAMILY NOTE (epsilon-invariance
 *   decomposition): the colloquial label 'dignified death' covers three
 *   structurally distinct constraints. This story authors epsilon=0.60 for
 *   the prohibition arrangement (victims: those denied exit; gains: diffuse
 *   moral-order enactment). The autonomy_primary sibling authors epsilon for
 *   a patient-authority arrangement whose victim set is vulnerable
 *   populations under pressure-to-die. The relational_autonomy sibling
 *   authors epsilon for a triad-procedural arrangement with its own safeguard
 *   overhead. Same terrain, three constraints, three epsilon values, three
 *   beneficiary/victim structures - linked via network.affects_constraints,
 *   not merged into one observable-dependent story. KEY AGENTS (by structural
 *   relationship): - terminally_suffering_patients: primary target
 *   (powerless/trapped) - bears coerced prolongation of dying; also
 *   structurally excluded from the forums that bind them -
 *   disabled_rights_community: target with identity-locked exit
 *   (organized/identity_locked) - carries the stakes most visibly; internally
 *   split between defending the assurance and chafing at the paternalism -
 *   poor_dying_patients: primary target (powerless/trapped) - cannot purchase
 *   the exits wealthier patients use - elderly_ready_to_die: target
 *   (moderate/trapped) - the 'finished with life' population the prohibition
 *   answers with continuation - compassionate_helpers: target
 *   (moderate/constrained) - family, nurses, physicians facing criminal
 *   exposure for mercy - religious_institutions: principal beneficiary
 *   (institutional/mobile) - collects doctrinal enforcement as general law -
 *   palliative_care_sector: secondary beneficiary (organized/constrained) -
 *   receives the mandate and funding argument of being the law's humane
 *   alternative - legislative_judicial_authorities: agenda setter
 *   (institutional/mobile) - maintains, amends, or strikes the arrangement -
 *   comparative_bioethics_scholarship: analytical observer - documents the
 *   cross-jurisdiction natural experiment
 *
 * KEY AGENTS:
 *   - terminally_suffering_patients: primary target (powerless/trapped) - bears the prohibition's core cost, coerced prolongation of unbearable dying; secondarily excluded from the deliberative forums that decide their fate
 *   - disabled_rights_community: target (organized/identity_locked) - the prohibition encodes an assurance their identity is bound to, while foreclosing options and imposing presumptive paternalism on them
 *   - poor_dying_patients: primary target (powerless/trapped) - bear the full weight because they cannot buy the foreign-clinic and legal-counsel exits available to the wealthy
 *   - elderly_ready_to_die: target (moderate/trapped) - the dependent elderly whose wish to die is answered with legally mandated continuation
 *   - compassionate_helpers: target (moderate/constrained) - family members and clinicians exposed to imprisonment for acts of assistance they understand as mercy
 *   - religious_institutions: principal beneficiary (institutional/mobile) - their doctrine governs nonbelievers via criminal statute, extending institutional reach at the point of maximal cosmic consequence
 *   - palliative_care_sector: secondary beneficiary (organized/constrained) - gains budgetary and moral standing as the arrangement's mandated humane alternative
 *   - legislative_judicial_authorities: agenda setter (institutional/mobile) - parliaments and courts that maintain, amend, or strike the prohibition; their cost of change is political, not material
 *   - comparative_bioethics_scholarship: analytical observer (analytical/analytical) - tracks the worldwide natural experiment and produces the evidence every seat selectively cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.6).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.74).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Intentional Death (Consent-Irrelevant)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical law/political philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'd18e4c05-dae9-40ec-ab97-1046228bfcc0').
narrative_ontology:cs_kernel_codification('d18e4c05-dae9-40ec-ab97-1046228bfcc0', distributed).
narrative_ontology:cs_authority_grounding('d18e4c05-dae9-40ec-ab97-1046228bfcc0', lineage).
narrative_ontology:cs_interpretation_layer_present('d18e4c05-dae9-40ec-ab97-1046228bfcc0').
narrative_ontology:cs_reading_relation('d18e4c05-dae9-40ec-ab97-1046228bfcc0', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('d18e4c05-dae9-40ec-ab97-1046228bfcc0', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('d18e4c05-dae9-40ec-ab97-1046228bfcc0', foundational, life_value_intrinsic_not_consent_indexed).
narrative_ontology:cs_axiom_status(life_value_intrinsic_not_consent_indexed, holdable).
narrative_ontology:cs_axiom_grounding('d18e4c05-dae9-40ec-ab97-1046228bfcc0', life_value_intrinsic_not_consent_indexed, deontological).
narrative_ontology:cs_axiom('d18e4c05-dae9-40ec-ab97-1046228bfcc0', secondary, consent_insufficient_to_license_intentional_death).
narrative_ontology:cs_axiom_status(consent_insufficient_to_license_intentional_death, holdable).
narrative_ontology:cs_axiom_grounding('d18e4c05-dae9-40ec-ab97-1046228bfcc0', consent_insufficient_to_license_intentional_death, deontological).
narrative_ontology:cs_reference_frame('d18e4c05-dae9-40ec-ab97-1046228bfcc0', transcendent_inviolability_framework).
narrative_ontology:cs_drift_state('d18e4c05-dae9-40ec-ab97-1046228bfcc0', secular_legal_pluralism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d18e4c05-dae9-40ec-ab97-1046228bfcc0', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, palliative_care_sector).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_suffering_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_rights_community).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, poor_dying_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_ready_to_die).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, compassionate_helpers).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, inviolability_of_innocent_life_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are dying with conditions they judge unbearable and ask for help ending their lives on their own schedule. Criminal law forbids anyone from providing that help. Their options reduce to enduring, refusing treatment, voluntarily stopping food and fluid, unaided and sometimes violent suicide, or - for those with money and time - relocating to a handful of foreign clinics before capacity to travel is lost. They hold no standing in the legislative committees, ethics boards, and review bodies that decide whether people in their position may be helped; they enter the record only as case studies argued over by others.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_suffering_patients, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, terminally_suffering_patients, excluded).

% Organizations of disabled people carry the arrangement's stakes most visibly. The prohibition encodes an assurance their communities fought for - that dependent lives will not be steered toward death - and many disabled advocates defend it fiercely. At the same time the same law forecloses the option for any disabled person who would someday choose a hastened death, and wraps disabled patients in a paternalism that presumes they cannot mean what they say. Their collective identity is bound to the assurance; stepping off it would mean re-fighting the devaluation battle on unfamiliar terrain, so the community's relationship to the arrangement is fused rather than chosen fresh.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_rights_community, payer,
    organized, generational, identity_locked, global).

% Bear the prohibition at full weight because they cannot purchase the exits wealthier patients use: no clinic trips abroad, no offshore arrangements, no counsel to navigate prosecutorial discretion or end-of-life law. Where assisted death is lawful elsewhere the option effectively exists along income lines; for them the law's implicit offer - care or suffering - arrives with underfunded hospice systems attached, and the suffering half of the offer is the one their circumstances deliver.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, poor_dying_patients, payer,
    powerless, immediate, trapped, national).

% Old adults who are not terminally ill in the narrow sense but are finished - tired of life, dependent, bereaved - are the population the prohibition addresses most bluntly: their wish to die is answered with legally mandated continuation. Some experience this as protection of their worth; others experience it as conscription into years of dependency they explicitly declined. Either way the decision is made for them by criminal law rather than with them, and their only lawful exits (refusing food and fluid) are slow and hard.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_ready_to_die, payer,
    moderate, immediate, trapped, national).

% Family members, friends, nurses, and physicians who respond to explicit requests for help dying face criminal exposure - up to fourteen years imprisonment in some jurisdictions - for acts they understand as mercy. Prosecutorial guidelines in some places filter out compassionate cases after investigation; in others prosecutions proceed and destroy careers and families. Their exit is unavailable in any real sense: the person asking is someone they love or care for, and declining the request does not return it to sender.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, compassionate_helpers, payer,
    moderate, biographical, constrained, national).

% Collect the enactment of their doctrine as general law: the inviolability norm they teach governs believers and nonbelievers alike through criminal statute, which extends institutional authority precisely where their cosmic claims carry the most consequence. They supply the prohibition's most durable electoral defense and its theological warrant, and they mobilize reliably whenever repeal approaches. Their doctrine does not require state enforcement to survive internally, but enforcement extends their reach far beyond their flock, and they can advocate or refrain at will.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, beneficiary,
    institutional, civilizational, mobile, global).

% Receives the mandate and much of the funding argument that flows from the prohibition: 'better care, not assisted death' casts hospice and palliative medicine as the answer the law requires, converting the arrangement into budget lines and moral standing for the sector. The sector is internally divided - many practitioners support legal assisted dying - but institutionally it is positioned as the prohibition's humane alternative and its standing depends partly on that positioning continuing.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_sector, beneficiary,
    organized, generational, constrained, national).

% Parliaments and courts maintain, amend, or strike the prohibition. Constitutional courts in several jurisdictions have overturned absolute bans as disproportionate to individual rights; legislatures elsewhere have reaffirmed them under religious and disability-community pressure, and several have legalized with safeguards. They set the terms of debate, commission inquiries, choose which exceptions exist, and decide prosecution policy. Their cost of changing the arrangement is political rather than material, and they have repeatedly demonstrated the change is available.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legislative_judicial_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Tracks the natural experiment the world is running: jurisdictions prohibiting, permitting with safeguards, and expanding eligibility over decades. Documents coercion and burden-framing reports, safeguard performance, palliative-access development, and eligibility-creep trajectories. Produces the evidence base that every other seat cites selectively; collects nothing from the arrangement and bears none of its costs.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, comparative_bioethics_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared, enforceable norm that intentional life-termination is impermissible, coordinating end-of-life trust: the elderly, disabled, and medically dependent can assume that no family member, clinician, or state actor will hasten their death, and medicine retains an unambiguous line separating care from killing.
% TRANSFER_FUNCTION: Moves decision authority over the timing and manner of death from the dying individual to the moral-legal order: the dying surrender exit options and absorb prolonged suffering, prosecuted helpers surrender liberty and livelihood, and in exchange the community receives an enacted guarantee that no life is intentionally ended regardless of anyone's request.
% ABSENT_VOICES: The dying themselves are structurally absent from the forums that bind them: legislative committees and review bodies hear institutional testimony from religious bodies, disability organizations, and medical associations, while the competent, suffering person whose death is at issue has no standing to assert their own claim except as a litigant attacking the rule. Also largely absent: prosecuted family members, who appear only as defendants, and poor patients, whose interests are argued by intermediaries with different priorities.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, end-of-life practice rearranges within years: assisted-dying statutes and clinical protocols emerge wherever prohibitions fall (the observed pattern in every jurisdiction that repealed), prosecutorial priorities dissolve, palliative-care funding arguments restructure around a no-longer-exclusive mandate, and religious political coalitions reorganize around the lost enforcement. The arrangement's disappearance removes a load-bearing wall of the current end-of-life settlement; the world does not stay put.
% FOUNDING_PROBLEM: The prohibition descends from religious and natural-law traditions codifying the inviolability of innocent human life, built to solve the problem of murder, infanticide, abandonment of the dependent, and the devaluation of lives deemed burdensome - long before medicine made requested death technically available.
% FOUNDING_PROBLEM_CORROBORATION: Disability-rights scholarship - outside the religious benefiting set - corroborates that the protective problem is real, documenting pressure and burden-framing incidents where assisted death is legal, while simultaneously disputing that absolute prohibition solves it. Parliamentary inquiry records and bioethics commission evidence attest both the live protective concern and the countervailing toll of imposed suffering under prohibition. No source outside the tradition that asserts it corroborates the stronger founding claim - that inviolability admits no consent exception whatsoever; that premise is attested only from within the benefiting tradition, and the statement of that absence is itself the signal.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: the arrangement transfers decision authority over death from the dying to the moral-legal order and delivers prolonged suffering to a concentrated population while its yield is diffusely consumed; the value sits in the manifest's 0.50-0.65 band and has risen across the interval as legal exit became technically trivial and publicly demanded. Suppression 0.74: persistence runs on criminal penalty (up to fourteen years in some jurisdictions), professional-license threat, and active closure of exit routes - this is a constraint held against expressed majority preference, which is the defining suppression signature. Accessibility_collapse 0.5: alternatives do not fully collapse - treatment refusal, voluntary stopping of eating and drinking, palliative sedation gray zones, underground aid networks, and Swiss-clinic travel for the wealthy persist - but every remaining alternative is either agonizing, illegal, or priced beyond most victims. Resistance 0.7: decades of organized campaigns, constitutional litigation, referendum victories, physician civil disobedience, and stable supermajority opinion. Theater_ratio 0.45: the protective apparatus (safeguard rhetoric, ethics review, 'care not killing' framing) performs protection while the delivered good is prohibition; the ratio has climbed as legalization evidence accumulated faster than protective delivery did. The measurement series run on one shared seven-point grid (1960-2025) with every tracked metric authored at every point. The suppression_requirement arc rises to a 2008 peak (era of aggressive prosecutions of helpers) then partially relaxes (prosecutorial discretion policies, declination guidelines) while the statutory prohibition itself persists - enforcement softened at the margins, not the core. Claimed type snare reflects the arrangement's OPERATION rather than its supporters' intentions: whatever the sincerity of protective motives, the arrangement's actual operation imposes concentrated suffering on identifiable populations, suppresses exits, and persists through enforcement against preference. Whether the protective core is genuine enough to force a tangled_rope recomputation is exactly what the protection_genuineness omega holds open.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute sharply different types from identical structural data. From the trapped payer seats (dying patients, poor dying, elderly ready to die) the arrangement is experienced as pure imposition: their exit options are non-existent or purchasable only at prices they cannot pay, so effective extraction lands near its full-target ceiling. From the religious beneficiary seat the same arrangement is the enactment of a moral cosmology - subsidy, not extraction. The disabled seat is the interesting divergence: identity_locked exit fuses community identity with the prohibition's protective assurance, damping the seat's computed extraction below what its dissenting members report, while its organized power gives it agenda influence no other payer seat has. The agenda-setter seat experiences the arrangement as a live political question it administers rather than bears. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations plus exit atoms drive the derivation: terminally_suffering_patients, poor_dying_patients, and elderly_ready_to_die carry trapped exit and sit nearest the full-target end; compassionate_helpers are constrained (they cannot decline the request without abandoning the person); disabled_rights_community are identity_locked, which pins them nearer the target end than their organized power alone would suggest. religious_institutions are declared beneficiaries with mobile exit - they champion the arrangement and can leave it at will - placing them near the beneficiary end. palliative_care_sector benefits but is constrained (embedded in the arrangement it is cast to justify), sitting moderately subsidized. legislative_judicial_authorities administer without collecting material rents. On the receipt surface: gain_flow is authored as 'diffuse' as an affirmative checked claim - every named seat was examined and none converts the extracted value (suffering borne, liberty forgone, mercy criminalized) into concentrated revenue or power; the arrangement's yield is an enacted moral order consumed diffusely by the believing community. Receipt is not benefit: religious_institutions hold role=beneficiary (they collect doctrinal enforcement and institutional reach) without being the seat the extraction accrues to. fixing_cost is 'cheap': the fixers (legislatures, constitutional courts) face political cost, not resource cost, and multiple jurisdictions have paid it successfully - the arrangement persists despite cheap fixing because its maintenance runs on coercion and identity, not on capture or fix-cost, which is precisely the snare persistence profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing wrongful killing and protecting dependent lives from devaluation - remains live at its core (the homicide prohibition rests on it) but is contested exactly at this arrangement's margin: whether requested, assisted death falls under the founding prohibition or under a different category altogether. founding_problem_status is therefore 'contested', paired with disappearance_verdict 'world_rearranges'; the mismatch consumer reads contested x world_rearranges and finds no dead-mandate flag - correctly, because the arrangement is not a zombie: it is vigorously defended, recently reaffirmed in several legislatures, and its protective mandate is asserted-live by its holders. Mandatrophy discipline prevents two opposite mislabels here: it blocks reading the arrangement as a decayed rope performing rituals (the enforcement is real, prosecutions happen, the theater is rhetorical inflation atop real coercion, not replacement of function by performance), and it blocks reading the sincere protective motive as proof of benign coordination (motive does not launder operation). The rising theater_ratio is watched, not decisive: if protective delivery continues to lag protective rhetoric, the arrangement drifts toward the theatrical-maintenance profile without yet inhabiting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the sanctity_primary reading of the dignified_death kernel; the autonomy_primary and relational_autonomy siblings instantiate different constraints over the same terrain. What exactly changes structurally if a sibling reading becomes the standing arrangement?',
    'Comparative classification across the three sibling stories: victim sets invert (populations pressured toward death under patient-authority regimes versus populations coerced into prolonged dying under prohibition), the beneficiary structure dissolves from a diffuse moral order into the choosing individual, and epsilon re-bases onto whichever arrangement actually stands.',
    'Adopting autonomy_primary would relocate the victim set to vulnerable populations under pressure-to-die and make this reading''s prohibition the contested alternative; the classification computed here is valid only while the prohibition is the standing arrangement under contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of the dignified_death kernel; sibling readings change victim and beneficiary structure wholesale.').

omega_variable(
    protection_genuineness,
    'Is the prohibition''s protective coordination function genuine (vulnerable people are actually protected better than under regulated legalization) or cover (the protection story masks the imposition of prolonged suffering)?',
    'Cross-jurisdiction outcome comparison: coercion and burden-framing incident rates, safeguard performance audits, and palliative-access trajectories under prohibition versus regulated-permissive regimes, controlling for baseline healthcare differences.',
    'Genuine and effective protection would push classification toward tangled_rope (real coordination plus asymmetric cost-bearing); protection that fails its own stated test confirms the snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_genuineness, empirical, 'Whether the protective story is delivered or decorative - the snare versus tangled_rope hinge.').

omega_variable(
    consent_irrelevance_load_bearing,
    'Is ''regardless of consent'' load-bearing in this reading''s prohibition, or would admitting competent-request exceptions collapse it into the autonomy sibling?',
    'Doctrinal analysis of proposed carve-outs (terminal-illness-only exceptions, judicial authorization schemes): if the tradition accepts any consent-based exception, the categorical premise is already overridden in practice.',
    'If consent exceptions are admissible, the constraint decomposes into a weaker safeguard regime and the categorical structure attributed here dissolves; if truly categorical, the full victim set stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_irrelevance_load_bearing, conceptual, 'Locates the exact structural element (consent-insufficiency) where this reading diverges from its siblings.').

omega_variable(
    enforcement_vs_conviction_persistence,
    'How much of the prohibition''s persistence is criminal enforcement operating against majority preference, versus settled conviction among its holders?',
    'Referendum and jurisdiction natural experiments: where direct democracy or constitutional courts bypassed legislative gatekeeping, prohibitions fell rapidly; measure residual conviction by the durability of post-legalization settlement and the absence of serious reversal movements.',
    'Enforcement-dominant persistence confirms the snare profile (a constraint held against its subjects'' expressed preference by coercion); conviction-dominant persistence would indicate a shared norm functioning as ordinary coordination for its holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_conviction_persistence, empirical, 'Persistence mechanism: coercion against preference or genuine shared conviction.').

omega_variable(
    dual_coercion_direction,
    'Which coercion dominates for the vulnerable populations this story names as victims: the pressure toward death they would face under legalization, or the coerced prolongation of dying they face under prohibition?',
    'Paired measurement: burden-framing and pressure incidents under permissive regimes versus involuntary-suffering duration and unrelieved-symptom prevalence under prohibitive ones, weighted by affected population size.',
    'If pressure-under-legalization dominates, the prohibition''s protective function is real and the victim set shrinks toward those who genuinely want exit; if prolongation dominates, the full victim set stands and epsilon sits at the top of its band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_coercion_direction, empirical, 'Direction of coercion on the vulnerable: toward death (legalization) or toward continued suffering (prohibition).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignified_death_sanctity_tr_t1960, dignified_death__sanctity_primary, theater_ratio, 1960, 0.16).
narrative_ontology:measurement_basis(dignified_death_sanctity_tr_t1960, observed).
narrative_ontology:measurement(dignified_death_sanctity_tr_t1972, dignified_death__sanctity_primary, theater_ratio, 1972, 0.21).
narrative_ontology:measurement_basis(dignified_death_sanctity_tr_t1972, observed).
narrative_ontology:measurement(dignified_death_sanctity_tr_t1984, dignified_death__sanctity_primary, theater_ratio, 1984, 0.26).
narrative_ontology:measurement_basis(dignified_death_sanctity_tr_t1984, observed).
narrative_ontology:measurement(dignified_death_sanctity_tr_t1996, dignified_death__sanctity_primary, theater_ratio, 1996, 0.32).
narrative_ontology:measurement_basis(dignified_death_sanctity_tr_t1996, observed).
narrative_ontology:measurement(dignified_death_sanctity_tr_t2008, dignified_death__sanctity_primary, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(dignified_death_sanctity_tr_t2008, observed).
narrative_ontology:measurement(dignified_death_sanctity_tr_t2017, dignified_death__sanctity_primary, theater_ratio, 2017, 0.42).
narrative_ontology:measurement_basis(dignified_death_sanctity_tr_t2017, observed).
narrative_ontology:measurement(dignified_death_sanctity_tr_t2025, dignified_death__sanctity_primary, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(dignified_death_sanctity_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(dignified_death_sanctity_be_t1960, dignified_death__sanctity_primary, base_extractiveness, 1960, 0.36).
narrative_ontology:measurement_basis(dignified_death_sanctity_be_t1960, observed).
narrative_ontology:measurement(dignified_death_sanctity_be_t1972, dignified_death__sanctity_primary, base_extractiveness, 1972, 0.42).
narrative_ontology:measurement_basis(dignified_death_sanctity_be_t1972, observed).
narrative_ontology:measurement(dignified_death_sanctity_be_t1984, dignified_death__sanctity_primary, base_extractiveness, 1984, 0.47).
narrative_ontology:measurement_basis(dignified_death_sanctity_be_t1984, observed).
narrative_ontology:measurement(dignified_death_sanctity_be_t1996, dignified_death__sanctity_primary, base_extractiveness, 1996, 0.51).
narrative_ontology:measurement_basis(dignified_death_sanctity_be_t1996, observed).
narrative_ontology:measurement(dignified_death_sanctity_be_t2008, dignified_death__sanctity_primary, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement_basis(dignified_death_sanctity_be_t2008, observed).
narrative_ontology:measurement(dignified_death_sanctity_be_t2017, dignified_death__sanctity_primary, base_extractiveness, 2017, 0.57).
narrative_ontology:measurement_basis(dignified_death_sanctity_be_t2017, observed).
narrative_ontology:measurement(dignified_death_sanctity_be_t2025, dignified_death__sanctity_primary, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(dignified_death_sanctity_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(dignified_death_sanctity_su_t1960, dignified_death__sanctity_primary, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement_basis(dignified_death_sanctity_su_t1960, observed).
narrative_ontology:measurement(dignified_death_sanctity_su_t1972, dignified_death__sanctity_primary, suppression_requirement, 1972, 0.63).
narrative_ontology:measurement_basis(dignified_death_sanctity_su_t1972, observed).
narrative_ontology:measurement(dignified_death_sanctity_su_t1984, dignified_death__sanctity_primary, suppression_requirement, 1984, 0.71).
narrative_ontology:measurement_basis(dignified_death_sanctity_su_t1984, observed).
narrative_ontology:measurement(dignified_death_sanctity_su_t1996, dignified_death__sanctity_primary, suppression_requirement, 1996, 0.77).
narrative_ontology:measurement_basis(dignified_death_sanctity_su_t1996, observed).
narrative_ontology:measurement(dignified_death_sanctity_su_t2008, dignified_death__sanctity_primary, suppression_requirement, 2008, 0.8).
narrative_ontology:measurement_basis(dignified_death_sanctity_su_t2008, observed).
narrative_ontology:measurement(dignified_death_sanctity_su_t2017, dignified_death__sanctity_primary, suppression_requirement, 2017, 0.76).
narrative_ontology:measurement_basis(dignified_death_sanctity_su_t2017, observed).
narrative_ontology:measurement(dignified_death_sanctity_su_t2025, dignified_death__sanctity_primary, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(dignified_death_sanctity_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three constraint stories per the epsilon-invariance principle: sanctity_primary (this file - the prohibition arrangement, epsilon 0.60, victims = populations coerced into prolonged dying), autonomy_primary (the patient-authority arrangement, whose victims are vulnerable populations under pressure-to-die), and relational_autonomy (the triad-procedural arrangement, with safeguard-overhead costs). The label 'dignity in dying' conflates these; measuring the prohibition with the autonomy reading's observables yields a different epsilon than measuring it with its own, which is the signal that they are different constraints. This story links to both siblings; the upstream/downstream citation pattern runs in both directions (each side cites the other's jurisdictions as cautionary or exemplary evidence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
